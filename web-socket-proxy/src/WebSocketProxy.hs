{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where


import           Safe

import           Data.Aeson
import qualified Data.ByteString.Char8      as C
import           Data.Foldable
import           Data.Monoid

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text                  as T
import           Data.Time

import           Control.Applicative
import           Control.Monad hiding (forM_)
import           Control.Monad.State (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Exception

import           Control.Concurrent
import           Control.Concurrent.STM

import           Snap.Core
import           Snap.Http.Server

import           Network.URI hiding (path)
import qualified Network.HTTP.Client     as HC
import qualified Network.HTTP.Client.TLS as HCTLS

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Network.Connection (TLSSettings(..))

import qualified Database.InfluxDB  as InfluxDB
import           Database.InfluxDB.TH

import           WebSocketProxy.Internal.Types
import           System.Environment

import           Network.HTTP.Conduit as C hiding (path, method)

import           Encounter.Logger
import           Encounter.Protocol.Internal

import           Prelude



type AccountId = Text
type ShardId   = Text


showText :: (Show a) => a -> Text
showText = T.pack . show



data Shard = Shard
    { shardHost :: Text
    , shardPort :: Int
    } deriving (Show)


shardAddress :: Shard -> Text
shardAddress Shard{..} = shardHost <> ":" <> showText shardPort


instance FromJSON Shard where
    parseJSON (Object o) = Shard <$> o .: "host" <*> o .: "port"
    parseJSON _          = fail "Shard"



data State = State
    { shardTunnels :: !(TMVar (Map ShardId Tunnel))
      -- ^ Connections to shards, keyed by the shard id. Whenever you want to
      --   manipulate the map, take it, change, and then put it back. This
      --   also serves as a lock so that we can safely open a unique
      --   connection to each shard from the IO monad.

    , httpManager :: !Manager
      -- A HTTP manager, in case somebody needs to make a HTTP request.

    , influxConfig :: !(Maybe InfluxDB.Config)
    }



data Tunnel = Tunnel
    { tunnelId :: !ShardId
      -- ^ The id of the shard to which the tunnel connects. Same as the key
      -- in the 'shardTunnels' map.

    , tunnelCreatedAt :: !UTCTime
      -- ^ When the tunnel was created. This is different from when the link
      -- was actually established, that is usually a few seconds after this.

    , tunnelShard :: !Shard
      -- ^ The shard which is at the other end of the tunnel.

    , tunnelClients :: !(TVar (Map AccountId Client))
      -- ^ All clients connected to the shard through this connection. When
      --   the connection is closed (eg. due to the shard crashing), these
      --   clients are automatically disconnected.

    , tunnelUpdatedAt :: !(TVar UTCTime)
      -- ^ Time when the tunnel clients map was last changed. This is used by
      --   the reaper thread to determine whether to close the tunnel after
      --   it's been inactive for a while.

    , tunnelLink :: !(TMVar Link)
      -- ^ The state of the connection to the shard.
    }



data Link
    = Unavailable
      -- ^ The connection is not available. Either because it hasn't been
      --   attempted yet, or because the connection was closed by the shard.

    | Connecting !ThreadId
      -- ^ A thread was created which will attempt to establishe the
      --   connection.

    | Established !ThreadId !Connection
      -- ^ The connection has been enstablished and packets are being sent
      --   over it.


instance Show Link where
    show Unavailable       = "Unavailable"
    show (Connecting  _)   = "Connecting"
    show (Established _ _) = "Established"


killLink :: Link -> IO ()
killLink Unavailable              = return ()
killLink (Connecting threadId)    = killThread threadId
killLink (Established threadId _) = killThread threadId


data Point = Point
    { pointValue :: !Int
    }


reportPoint :: State -> Text -> Int -> IO ()
reportPoint State{..} series value =
    case influxConfig of
        Nothing -> return ()
        Just cfg -> void $ forkIO $ do
            InfluxDB.postWithPrecision cfg "rmx" InfluxDB.SecondsPrecision $ InfluxDB.withSeries series $ do
                InfluxDB.writePoints $ Point value




data Client = Client
    { clientAccountId   :: !AccountId
    , clientRoutingInfo :: !RoutingInfo
    , clientThread      :: !ThreadId
    , clientConnection  :: !Connection
    }



-- | Update the tunnel timestamp to reflect that it has recently been used.
touchTunnel :: Tunnel -> IO ()
touchTunnel tunnel = do
    now <- getCurrentTime
    atomically $ writeTVar (tunnelUpdatedAt tunnel) now


receiveMessage :: FromJSON a => Connection -> IO a
receiveMessage connection = receiveData connection >>= return . fromJust . decode

-- | Encode a value into JSON and send it down the sink.
sendJSON :: (ToJSON a) => Connection -> a -> IO ()
sendJSON connection = sendTextData connection . encode



-- | Create a new connection to the given shard. This function is only used
--  when there isn't already an open connection available.
createTunnel :: ShardId -> Shard -> IO Tunnel
createTunnel shardId shard = do
    logger $ mconcat
        [ "Creating new tunnel to shard "
        , shardId
        , " address "
        , shardAddress shard
        ]

    now        <- getCurrentTime
    clients    <- newTVarIO M.empty
    updatedAt  <- newTVarIO =<< getCurrentTime
    link       <- newTMVarIO Unavailable

    return $ Tunnel
        { tunnelId        = shardId
        , tunnelCreatedAt = now
        , tunnelShard     = shard
        , tunnelClients   = clients
        , tunnelUpdatedAt = updatedAt
        , tunnelLink      = link
        }


disconnectTunnelClients :: Tunnel -> IO ()
disconnectTunnelClients tunnel = do
    clients <- atomically $ readTVar $ tunnelClients tunnel

    logger $ mconcat
        [ "Disconnecting "
        , showText (M.size clients)
        , " clients from "
        , shardAddress (tunnelShard tunnel)
        ]

    forM_ clients $ killThread . clientThread



-- | Function which runs forever, reads from the connection and dispatches
-- messages to clients.
tunnelReader :: State -> Tunnel -> Connection -> IO ()
tunnelReader state Tunnel{..} connection = do
    logger $ "Established tunnel connection to " <> shardAddress tunnelShard

    -- Update the link state to indicate that the connection has been
    -- established.
    threadId <- myThreadId
    void $ atomically $ swapTMVar tunnelLink (Established threadId connection)

    -- Periodicall send a Ping control message.
    forkPingThread connection 30

    -- Forever read packets from the shard and dispatch them to clients.
    forever $ do
        packet  <- receiveMessage connection
        clients <- atomically $ readTVar tunnelClients

        case M.lookup (spktAccountId packet) clients of
            Nothing -> return ()
            Just client -> sendReply packet client

  where
    sendReply packet client = do
        when (clientRoutingInfo client == spktRoutingInfo packet) $ do
            sendJSON (clientConnection client) $ spktMessage packet
            reportPoint state "proxy.packet.tx.count" 1
            -- (fromIntegral $ LC.length $ encode $ spktMessage packet)



acquireTunnels :: State -> IO (Map ShardId Tunnel)
acquireTunnels state = atomically $ takeTMVar (shardTunnels state)

releaseTunnels :: State -> Map ShardId Tunnel -> IO ()
releaseTunnels state = atomically . putTMVar  (shardTunnels state)



-- | If there is no thread associated with the tunnel, create one. This must
-- be done while still holding the tunnels lock.
ensureLink :: State -> Tunnel -> IO ()
ensureLink state tunnel = do
    link <- atomically $ takeTMVar (tunnelLink tunnel)

    newLink <- case link of
        Unavailable -> do
            logger $ "Link is unavailable, creating a new one..."
            threadId <- forkIO $ flip catch cleanup $ do
                runClient host port "/" (tunnelReader state tunnel)
            return $ Connecting threadId

        x -> return x

    atomically $ putTMVar (tunnelLink tunnel) newLink

  where

    cleanup :: SomeException -> IO ()
    cleanup e = do
        logger $ "Cleaning up tunnel connection due to " <> showText e
        void $ atomically $ swapTMVar (tunnelLink tunnel) Unavailable
        disconnectTunnelClients tunnel

    -- FIXME: Not sure why exactly this is necessary. But 'runClient' does not
    --        work when connecting to "localhost".

    fixupHost "localhost" = "127.0.0.1"
    fixupHost x           = x

    shard = tunnelShard tunnel
    host  = T.unpack $ fixupHost $ shardHost shard
    port  = shardPort shard


getTunnel :: State -> RoutingInfo -> IO Tunnel
getTunnel state routingInfo = do
    logger $ "getTunnel " <> showText routingInfo

    (shardId, shard) <- getShard routingInfo

    tunnels <- acquireTunnels state
    tunnel  <- case M.lookup shardId tunnels of
        Nothing -> createTunnel shardId shard
        Just x  -> return x
    ensureLink state tunnel

    releaseTunnels state $ M.insert shardId tunnel tunnels

    return tunnel

  where

    getShard :: RoutingInfo -> IO (ShardId, Shard)
    getShard (RoutingInfo "game" gameId) = do
        mbApi <- lookupEnv "API"
        let api = maybe "https://api.rmx.im" T.pack mbApi
        addr <- getJSON (httpManager state) $
            api <> "/games/" <> gameId <> "/shardAddress"
        case addr of
            Nothing -> error "getShard failed"
            Just (host, port) -> return (host <> ":" <> showText port, Shard host port)

    getShard _ = error "Unhandled RoutingInfo"


getJSON :: (FromJSON a) => Manager -> Text -> IO (Maybe a)
getJSON httpManager url = do
    req <- acceptJSON <$> parseUrl (T.unpack url)
    body <- responseBody <$> httpLbs req httpManager
    return $ decode body

  where

    acceptJSON req = req { C.requestHeaders = acceptHeader : C.requestHeaders req }
    acceptHeader = ("Accept","application/json")


app :: State -> PendingConnection -> IO ()
app state pendingConnection = do
    let RequestHead{..} = pendingRequest pendingConnection

    case requestPath of
        "/" -> do
            logger $ "game client connected"

            -- TODO: Also check the origin
            connection <- acceptRequest pendingConnection

            -- Keep the connection alive by periodically sending pings.
            forkPingThread connection 30

            handleClientConnection state connection

        _ -> do
            rejectRequest pendingConnection "Unknown path"



-- | The main function which handles a new connection from a client.
handleClientConnection :: State -> Connection -> IO ()
handleClientConnection state connection =

    -- Close the connection to the client when any exception is thrown.
    flip catch exceptionHandler $ do
        threadId <- myThreadId

        -- The first message includes the accountId and routing info.
        (CMsgAuth accountId routingInfo) <- receiveMessage connection
        reportPoint state "proxy.packet.rx.count" 1

        -- Once the client is authenticated and we know where he wants to connect
        -- to, we establish a tunnel to the shard
        tunnel <- getTunnel state routingInfo

        -- And finally attach the client to the tunnel.
        let client = Client accountId routingInfo threadId connection
        attachClient state tunnel client

  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler e = do
        logger $ "Exception during initial handshake " <> showText e
        closeConnectionWith connection $ "Protocol Error: " <> showText e



-- | Attach a client to the tunnel and pump messages from the client to the
-- tunnel. Makes sure to clean up the client when it disconnects.
attachClient :: State -> Tunnel -> Client -> IO ()
attachClient state tunnel client =

    -- When an exception is thrown, we have to unregister the client from the
    -- tunnel
    flip catch (disconnectClient tunnel client) $ do
        logger $ "Attaching client to the tunnel"

        -- Register the client in the tunnel client list.
        atomically $ modifyTVar' (tunnelClients tunnel) $
            M.insert (clientAccountId client) client

        -- This 'readTMVar' blocks until the connection to the shard is
        -- established.
        connection <- atomically $ do
            link <- readTMVar $ tunnelLink tunnel
            case link of
                Unavailable       -> retry
                (Connecting _)    -> retry
                (Established _ x) -> return x

        -- The first message we send to the shard must be CONNENCT.
        let packet = ClientPacket accountId routingInfo (toJSON CMsgConnect)
        sendJSON connection packet

        -- Forever pump packets from the client to the shard.
        forever $ do
            msg <- receiveMessage $ clientConnection client
            touchTunnel tunnel

            reportPoint state "proxy.packet.rx.count" 1

            sendJSON connection $ ClientPacket accountId routingInfo msg

  where
    accountId   = clientAccountId client
    routingInfo = clientRoutingInfo client


closeConnectionWith :: Connection -> Text -> IO ()
closeConnectionWith connection text = do
    sendJSON connection $ SMsgDisconnect text
    sendClose connection text


disconnectClient :: Tunnel -> Client -> SomeException -> IO ()
disconnectClient tunnel client e = do
    logger $ "Disconnecting client " <> clientAccountId client

    atomically $ modifyTVar' (tunnelClients tunnel) (M.delete $ clientAccountId client)
    closeConnectionWith (clientConnection client) (showText e)



-- The reaper thread runs in the background and closes idle tunnel collection.
reaperThread :: State -> IO ()
reaperThread state = forever $ do
    -- First sleep for a bit
    threadDelay $ 1000 * 1000

    -- Get all tunnels which have been idle for too long.
    now     <- getCurrentTime
    tunnels <- M.toList <$> (atomically $ readTMVar $ shardTunnels state)

    idleTunnels <- (flip filterM) tunnels $ \(_, tunnel) -> do
        updatedAt <- atomically $ readTVar $ tunnelUpdatedAt tunnel
        noClientsConnected <- isTunnelIdle tunnel
        return $ noClientsConnected && diffUTCTime now updatedAt > fromInteger 10

    let numIdleTunnels = length idleTunnels
    when (numIdleTunnels > 0) $
        logger $ "Cleaning up " <> (showText numIdleTunnels) <> " idle tunnels"

    forM_ idleTunnels $ \(key, tunnel) -> do
        lock   <- acquireTunnels state
        isIdle <- isTunnelIdle tunnel

        if isIdle
            then do
                releaseTunnels state $ M.delete key lock

                -- logger $ "Tunnel to " ++ (show $ shardId $ tunnelShard tunnel) ++ " is idle, closing..."

                link <- atomically $ readTMVar $ tunnelLink tunnel
                killLink link

            else do
                releaseTunnels state lock

  where

    isTunnelIdle tunnel = atomically $ do
        x <- readTVar $ tunnelClients tunnel
        return $ M.null x


status :: State -> Snap ()
status state = method GET $ path "status" $ do
    st <- liftIO $ atomically $ readTMVar $ shardTunnels state
    forM_ (M.toList st) $ \(key, tunnel) -> do
        link         <- liftIO $ atomically $ readTMVar (tunnelLink tunnel)
        updatedAt    <- liftIO $ atomically $ readTVar $ tunnelUpdatedAt tunnel
        now          <- liftIO $ getCurrentTime
        let dt = diffUTCTime now updatedAt

        writeText $ "Tunnel " <> key <> "\n"
        writeText $ " Shard: " <> shardAddress (tunnelShard tunnel) <> "\n"
        writeText $ " Last Updated At: " <> showText dt <> " ago\n"
        writeText $ " Link: " <> showText link <> "\n"
        writeText $ " Clients:\n"
        clients <- liftIO $ atomically $ readTVar $ tunnelClients tunnel
        forM_ (M.toList clients) $ \(accountId, client) -> do
            let ri     = clientRoutingInfo client
            let riType = routingType ri
            let riId   = routingId ri

            writeText $ "  " <> accountId <> ": " <> riType <> "/" <> riId <> "\n"


favicon :: Snap ()
favicon = method GET $ path "favicon.ico" $ do
    return ()


loadInfluxConfig :: Manager -> IO (Maybe InfluxDB.Config)
loadInfluxConfig httpManager = runMaybeT $ do
    uri <- MaybeT $ lookupEnv "INFLUXDB"
    URI{..} <- MaybeT $ return $ parseURI uri
    URIAuth{..} <- MaybeT $ return $ uriAuthority
    (username, password) <- MaybeT $ case T.splitOn ":" (T.pack $ init uriUserInfo) of
        [a,b] -> return $ Just (a,b)
        _     -> return Nothing


    port <- MaybeT $ return $ readMay (tail uriPort)
    serverPool <- MaybeT $ do
        pool <- InfluxDB.newServerPool
            (InfluxDB.Server (T.pack uriRegName) port False)
            []

        return $ Just pool


    MaybeT $ return $ Just $ InfluxDB.Config
        (InfluxDB.Credentials username password)
        serverPool
        httpManager



main :: IO ()
main = do
    logger $ "web-socket-proxy starting up"

    httpManager <- HC.newManager $ HCTLS.mkManagerSettings
        (TLSSettingsSimple True True True) Nothing

    influxConfig <- loadInfluxConfig httpManager

    state <- State
        <$> newTMVarIO M.empty
        <*> pure httpManager
        <*> pure influxConfig

    -- The reaper closes tunnels which have been idle for too long.
    void $ forkIO $ reaperThread state

    config <- createConfig
    httpServe config (favicon <|> status state <|> runWebSocketsSnap (app state))

  where

    stdioLogger = ConfigIoLog C.putStrLn
    createConfig = do
        config <- commandLineConfig defaultConfig
        return $ setAccessLog stdioLogger $ setErrorLog stdioLogger config



deriveSeriesData defaultOptions
  { fieldLabelModifier = stripPrefixLower "point" }
  ''Point
