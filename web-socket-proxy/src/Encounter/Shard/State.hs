module Encounter.Shard.State where

import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Time.Clock

import           Control.Monad.State
import           Control.Concurrent
import           Control.Concurrent.STM

import           Network.WebSockets

import           Encounter.MongoDB.Pipe
import           Encounter.Protocol.Internal

import           Encounter.Shard.Types

import           Encounter.Shard.Target.Game
import           Encounter.Shard.Target.Wardrobe


data ShardState = ShardState
    { _pipe :: Pipe
    , _targets :: Map RoutingInfo Target
    }

newtype ShardM a = ShardM
    { unShard :: StateT ShardState IO a
    }

instance Monad ShardM where
    (ShardM a) >>= f = ShardM $ do
        res <- a
        unShard $ f res

    return = ShardM . return

instance MonadIO ShardM where
    liftIO = ShardM . liftIO

instance Functor ShardM where
    fmap = liftM

sget :: ShardM ShardState
sget = ShardM get

smodify :: (ShardState -> ShardState) -> ShardM ()
smodify f = ShardM $ modify f

getTargetAction :: RoutingInfo -> Pipe -> RoutingInfo -> TargetAction
getTargetAction (RoutingInfo "game"     _) = gameAction
getTargetAction (RoutingInfo "wardrobe" _) = wardrobeAction
getTargetAction _ = \_ _ _ _ -> return ()

getTarget :: RoutingInfo -> ShardM Target
getTarget routingInfo = do
    pipe <- liftM _pipe sget
    targets <- liftM _targets sget

    case M.lookup routingInfo targets of
        Just target -> return target
        Nothing -> do
            let ta = getTargetAction routingInfo pipe
            target <- liftIO $ createTarget routingInfo $ (ta routingInfo)
            smodify $ \s -> s { _targets = M.insert routingInfo target (_targets s) }
            return target

queueMessageFromClient :: Target -> String -> Value -> Connection -> ShardM ()
queueMessageFromClient target accountId msg connection = liftIO $ do
    time <- getCurrentTime

    atomically $ do
        modifyTVar (clientConnections target) (M.insert accountId connection)
        writeTQueue (clientMessageQueue target) (accountId, time, msg)


type ConnectionMap = TVar (Map String Connection)

data Target = Target
    { epoch :: UTCTime
      -- ^ Time when the target was created. Times in the 'clientMessageQueue'
      --   are relative to the epoch.

    , clientMessageQueue :: ClientMessageQueue
      -- ^ Messages from clients are put into this queue to be processed by
      --   the target thread.

    , targetThread :: ThreadId
      -- ^ Thread that processes messages from the client queue. Each target
      --   type (game, wardrobe, replay) implements its own thread action.

    , replyThread :: ThreadId
      -- ^ The reply thread reads messages from the server queue and
      --   dispatches the messages to client sinks.

    , clientConnections :: ConnectionMap
      -- ^ Sinks of each client that is connected to the target.
    }



-- | Create a new target which uses the given action to process client
--   messages.
createTarget :: RoutingInfo -> TargetAction -> IO Target
createTarget targetSpec targetAction = do
    now           <- getCurrentTime

    clientQueue   <- newTQueueIO
    serverQueue   <- newTQueueIO
    sinks         <- newTVarIO M.empty

    targetThread' <- forkIO $ targetAction clientQueue serverQueue
    replyThread'  <- forkIO $ replyThreadAction targetSpec serverQueue sinks

    return $ Target now clientQueue targetThread' replyThread' sinks


-- | Read from the queue and dispatch the messages to the sinks.
replyThreadAction :: RoutingInfo -> ServerMessageQueue -> ConnectionMap -> IO ()
replyThreadAction targetSpec queue connections = forever $ do
    (playerId, msg) <- atomically $ readTQueue queue
    c <- atomically $ readTVar connections
    case M.lookup playerId c of
        Nothing -> return ()
        Just connection -> do
            let packet = ServerPacket playerId targetSpec msg
            sendTextData connection $ encode packet
