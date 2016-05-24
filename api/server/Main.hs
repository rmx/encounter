{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main, replAvers) where


import           Control.Applicative
import           Control.Concurrent
import           Control.Exception hiding (Handler, catches)
import           Control.Monad.State
import           Control.Monad.CatchIO hiding (catch)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import qualified Data.Vector as V
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8  as C
import           Data.IORef
import qualified Data.OrdPSQ            as PSQ
import qualified Data.Map               as M
import           Data.Time
import           Data.Aeson
import           Data.List
import           Data.Monoid

import           System.IO
import           System.Directory
import           System.Environment

import           Network.BSD

import           Snap

import           Routes
import           Types
import           Avers
import           Queries

import qualified Google.Cloud                as G
import qualified Google.Cloud.Storage        as G
import qualified Google.Cloud.Internal.Token as G

import           Network.URI
import           Network.Connection (TLSSettings(..))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCTLS

import qualified Database.InfluxDB.Writer as InfluxDB

import           Storage.ObjectTypes
import           Storage.Objects.Game.Types
import           Storage.Objects.Shard.Types

import           GHC.Stats (GCStats(..), getGCStats)


databaseConfig :: IO URI
databaseConfig = do
    uri <- fromMaybe "//localhost/rmx" <$> lookupEnv "RETHINKDB"
    return $ fromJust $ parseRelativeReference uri


loadInfluxConfig :: HC.Manager -> IO (Maybe InfluxDB.Handle)
loadInfluxConfig httpManager = do
    mbUrl <- lookupEnv "INFLUXDB"
    case mbUrl of
        Nothing -> return Nothing
        Just url -> do
            let config = InfluxDB.Config url
            hres <- InfluxDB.newHandle config httpManager
            case hres of
                Left _ -> return Nothing
                Right h -> return (Just h)


saveLocalBlob :: BlobId -> Text -> ByteString -> IO (Either AversError ())
saveLocalBlob (BlobId blobId) _contentType content = do
    createDirectoryIfMissing False "blobs"
    let filePath = ("blobs/" ++ T.unpack blobId)
    fileExists <- doesFileExist filePath
    unless fileExists $ BL.writeFile filePath content
    return $ Right ()

saveGoogleCloud :: G.Handle -> String -> BlobId -> Text -> ByteString -> IO (Either AversError ())
saveGoogleCloud h bucketName (BlobId blobId) contentType content = do
    let bucket = G.Bucket bucketName
    res <- G.evalCloud h $ do
        G.uploadMedia bucket (G.Name $ T.unpack blobId) (BL.toStrict content) (T.encodeUtf8 contentType)
    return $ case res of
        Left e -> Left $ AversError $ T.pack $ show e
        Right r -> Right r


createBlobStorageConfig :: HC.Manager -> IO (BlobId -> Text -> ByteString -> IO (Either AversError ()))
createBlobStorageConfig httpManager = do
    mbBucket <- lookupEnv "BUCKET"
    case mbBucket of
        Nothing -> return saveLocalBlob
        Just b -> do
            h <- G.mkHandle httpManager G.defaultMetadataToken
            return $ saveGoogleCloud h b


enableCors :: RequestHandler ()
enableCors = do
    origin <- getHeader "Origin" <$> getRequest
    case origin of
        Nothing -> return ()
        Just x -> do
            -- CORS (http://www.w3.org/TR/cors/)
            modifyResponse $ setHeader "Access-Control-Allow-Origin"      x
            modifyResponse $ setHeader "Access-Control-Allow-Credentials" "true"
            modifyResponse $ setHeader "Access-Control-Allow-Headers"     "Content-Type, X-Requested-With"
            modifyResponse $ setHeader "Access-Control-Allow-Methods"     "GET, POST, PATCH, DELETE"
            modifyResponse $ setHeader "Access-Control-Max-Age"           "3600"

            -- This is needed so that the browser doens't cache the CORS
            -- headers across origins (see http://www.w3.org/TR/cors/#resource-implementation)
            modifyResponse $ setHeader "Vary" "Origin"

            -- Resource Timing (http://www.w3.org/TR/resource-timing/)
            modifyResponse $ setHeader "Timing-Allow-Origin" x


handleRequestException :: RequestHandlerException -> RequestHandler ()
handleRequestException (BadRequest x)          = sendErrorResponse 400 x
handleRequestException Unauthorized            = sendErrorResponse 401 "unauthorized"
handleRequestException Forbidden               = sendErrorResponse 403 "forbidden"
handleRequestException NotFound                = sendErrorResponse 404 "not-found"
handleRequestException (InternalServerError x) = sendErrorResponse 500 x

handleExceptions :: RequestHandler () -> RequestHandler ()
handleExceptions m = m `catches`
    [ Handler handleRequestException
    ]


emitMeasurementImpl :: Text -> InfluxDB.Handle -> Measurement -> Double -> IO ()
emitMeasurementImpl machine h m value = void $ do
    let series = T.pack $ mconcat $ intersperse "." $ measurementLabels m
        tags   = M.fromList [("component","api"),("machine",machine)]
        values = M.fromList [("value", InfluxDB.F value)]

    InfluxDB.writePoint h series tags values

writeGhcStats :: InfluxDB.Handle -> Text -> GCStats -> Text -> (GCStats -> InfluxDB.Value) -> IO ()
writeGhcStats h machine stats name f = do
    let series = "haskell.rts.gc." <> name
        tags   = M.fromList [("component","api"),("machine",machine)]
        values = M.fromList [("value", f stats)]

    InfluxDB.writePoint h series tags values

createAversHandle :: HC.Manager -> IO Avers.Handle
createAversHandle httpManager = do
    dbURI <- databaseConfig
    icfg <- loadInfluxConfig httpManager
    bsc  <- createBlobStorageConfig httpManager

    machine <- T.pack <$> getHostName

    Right h <- newState $ Avers.Config dbURI bsc
        [ SomeObjectType accountObjectType
        , SomeObjectType encounterObjectType
        , SomeObjectType gameObjectType
        , SomeObjectType iconObjectType
        , SomeObjectType modelObjectType
        , SomeObjectType particleeffectObjectType
        , SomeObjectType skyboxObjectType
        , SomeObjectType shardObjectType
        , SomeObjectType soundObjectType
        , SomeObjectType tileObjectType
        ]
        (maybe (\_ _ -> return ()) (emitMeasurementImpl machine) icfg)


    case icfg of
        Nothing -> return ()
        Just ih -> void $ forkIO $ forever $ do
            stats <- getGCStats

            -- Counters
            writeGhcStats ih machine stats "numGCs"           (InfluxDB.I . numGcs)
            writeGhcStats ih machine stats "bytesAllocated"   (InfluxDB.I . bytesAllocated)
            writeGhcStats ih machine stats "bytesCopied"      (InfluxDB.I . bytesCopied)


            -- Gauges
            writeGhcStats ih machine stats "maxBytesUsed"     (InfluxDB.I . maxBytesUsed)
            writeGhcStats ih machine stats "currentBytesUsed" (InfluxDB.I . currentBytesUsed)

            writeGhcStats ih machine stats "overhead"         (\s -> InfluxDB.F $ gcCpuSeconds s / cpuSeconds s)

            threadDelay $ 60 * 1000 * 1000


    -- Bootstrap the Avers database
    res <- liftIO $ evalAvers h Avers.bootstrap
    case res of
        Left e -> error $ show e
        Right _ -> return ()

    return h


attachBackgroundWorkers :: Avers.Handle -> IO ()
attachBackgroundWorkers h = do
    -- Finish open games which either:
    --  - have content which we can't parse.
    --  - run on a shard which doesn't exist.
    --  - are older than 24 hours.

    void $ forkIO $ forever $
        reapGames h `catch` ignoreException


ignoreException :: SomeException -> IO ()
ignoreException e = do
    putStrLn $ "ignoreException: " ++ show e



reapGames :: Avers.Handle -> IO ()
reapGames h = do
    threadDelay (60 * 1000 * 1000)
    now <- getCurrentTime

    Right openGames <- evalAvers h openGamesExp
    forM_ (V.toList openGames) $ \gameId -> do
        let gameId' = BaseObjectId gameId

        let finishGame = void $ evalAvers h $ do
                Snapshot{..} <- Avers.lookupLatestSnapshot gameId'
                Avers.applyObjectUpdates gameId' snapshotRevisionId rootObjId
                    [ Set "stage" (Just $ toJSON $ Crashed)
                    ]
                    False

        gameResult <- evalAvers h $ objectContent gameId'
        case gameResult of
            Left e -> do
                -- If fetching the game fails, we prune it from the
                -- database. Radical, I know...
                putStrLn $ "Could not fetch game content: " ++ show e
                void $ evalAvers h $ do
                    deleteObject gameId
                    pruneObject gameId

            Right game -> do
                let shardId = gameShardId game

                shardResult <- evalAvers h $ objectContent (BaseObjectId shardId)
                case shardResult of
                    Left e -> do
                        putStrLn $ "Could not fetch game shard: " ++ show e
                        finishGame

                    Right shard -> do
                        -- Shard is ok. We don't check whether the shard
                        -- is alive or not.
                        let _addr = shardAddress shard

                        -- If the game was created more than a day ago, we
                        -- finish it.
                        Right obj <- evalAvers h $ Avers.lookupObject gameId

                        let gameAge = diffUTCTime now (objectCreatedAt obj)
                        when (gameAge > 60 * 60 * 2) $ do
                            finishGame


createShardRegistry :: IO ShardRegistry
createShardRegistry = do
    ior <- newIORef (PSQ.empty)

    -- Reap stale shards from the registry.
    void $ forkIO $ forever $ do
        shardReaper ior `catch` ignoreException

    return ior

  where
    shardReaper ior = do
        threadDelay (1000 * 1000)

        now <- getCurrentTime
        void $ atomicModifyIORef' ior $ \reg ->
            (PSQ.fromList $ filter (\(_,_,v) -> diffUTCTime now v < 10) $ PSQ.toList reg, ())


createPeerRegistry :: IO PeerRegistry
createPeerRegistry = do
    ior <- newIORef M.empty

    -- Reap stale shards from the registry.
    void $ forkIO $ forever $ do
        peerReaper ior `catch` ignoreException

    return ior

  where
    peerReaper ior = do
        threadDelay (1000 * 1000)

        now <- getCurrentTime
        void $ atomicModifyIORef' ior $ \reg ->
            (M.fromList $ cleanPeers now $ M.toList reg, ())

    cleanPeers :: UTCTime -> [(ObjId, [Peer])] -> [(ObjId, [Peer])]
    cleanPeers now l =
        filter (\(_, peers) -> length peers > 0) $
        map (\(k, peers) -> (k, filter (\Peer{..} -> diffUTCTime now peerUpdatedAt < 60) peers)) $
        l



mainSnaplet :: Avers.Handle -> ShardRegistry -> PeerRegistry -> SnapletInit AppState AppState
mainSnaplet h sr pr = makeSnaplet "rmx" "rmx server" Nothing $ do
    addRoutes $ map (\(m, p, action) -> (p, method m action)) routes

    wrapSite $ \site -> do
        enableCors
        handleExceptions $ method OPTIONS (return ()) <|> site

    return $ AppState h sr pr



main :: IO ()
main = do

    -- The global HTTP mananger. With custom TLS settings to disable certificate
    -- validation, see https://github.com/vincenthz/hs-tls/issues/105.
    httpManager <- HC.newManager $ HCTLS.mkManagerSettings
        (TLSSettingsSimple True True True) Nothing

    h <- createAversHandle httpManager
    attachBackgroundWorkers h

    sr <- createShardRegistry
    pr <- createPeerRegistry

    -- Start the web server (using custom snap config).
    snapConfig <- do
        let stdioLogger = ConfigIoLog $ \msg -> do
                C.putStrLn msg
                hFlush stdout

        config <- commandLineConfig defaultConfig
        return $ setAccessLog stdioLogger $ setErrorLog stdioLogger config

    serveSnaplet snapConfig $ mainSnaplet h sr pr



replAvers :: Avers a -> IO (Either AversError a)
replAvers m = do
    -- The global HTTP mananger. With custom TLS settings to disable certificate
    -- validation, see https://github.com/vincenthz/hs-tls/issues/105.
    httpManager <- HC.newManager $ HCTLS.mkManagerSettings
        (TLSSettingsSimple True True True) Nothing

    h <- createAversHandle httpManager
    evalAvers h m
