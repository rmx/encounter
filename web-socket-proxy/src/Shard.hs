module Main where

import           Data.Aeson
import           Data.Maybe
import qualified Data.Map as M
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Concurrent
import           Control.Concurrent.STM

import           Snap.Http.Server
import           Network.WebSockets
import           Network.WebSockets.Snap

import           Encounter.Utils

import           Encounter.Protocol.Internal
import           Encounter.MongoDB.Pipe
import           Encounter.MongoDB.Models

import           Encounter.Shard.State


type SupervisorQueuePacket = (Connection, String, RoutingInfo, Value)
type SupervisorQueue = TQueue SupervisorQueuePacket


handleIncomingPacket :: SupervisorQueuePacket -> ShardM ()
handleIncomingPacket (sink, accountId', routingInfo, msg) = do
    target <- getTarget routingInfo
    queueMessageFromClient target accountId' msg sink


supervisorThread :: Pipe -> SupervisorQueue -> IO ()
supervisorThread pipe queue = processNextMessage $ ShardState pipe M.empty
  where
    processNextMessage :: ShardState -> IO ()
    processNextMessage s = do
        packet <- atomically $ readTQueue queue
        let (ShardM m) = handleIncomingPacket packet
        (_, s') <- runStateT m s
        processNextMessage s'


app :: SupervisorQueue -> PendingConnection -> IO ()
app supervisorQueue pendingConnection = do
    connection <- acceptRequest pendingConnection
    forever $ do
        packet <- receivePacket connection
        maybe (return ()) (writeMessage connection) packet

  where

    receivePacket c = decodeClientPacket <$> receiveData c
    writeMessage connection (ClientPacket playerId targetSpec msg) = do
        liftIO $ atomically $ writeTQueue supervisorQueue (connection, playerId, targetSpec, msg)


main :: IO ()
main = do
    config <- commandLineConfig defaultConfig

    fqdn <- fromMaybe "localhost" <$> fullyQualifiedDomainName
    let port = fromMaybe 5555 $ getPort config

    pipe <- openMongoDB
    shardId' <- findShard pipe fqdn port

    void $ forkIO $ forever $ updateHeartbeat pipe shardId' >> threadDelay 1000000

    supervisorQueue <- newTQueueIO
    void $ forkIO $ supervisorThread pipe supervisorQueue

    httpServe config $ runWebSocketsSnap (app supervisorQueue)
