module Encounter.MongoDB.Models.Shard where

import Prelude hiding (lookup)
import Data.Time.Clock
import Control.Applicative
import Database.MongoDB hiding (host)
import Encounter.MongoDB.Types


data Shard = Shard
    { shardId :: ObjectId
    , shardHost :: String
    , shardPort :: Int
    , shardHeartbeat :: UTCTime
    , shardCapabilities :: [ String ]
    } deriving (Show)


instance FromBSON Shard where
    fromBSON x = Shard
        <$> lookup "_id" x
        <*> lookup "host" x
        <*> lookup "port" x
        <*> lookup "heartbeat" x
        <*> lookup "capabilities" x


-- | Find a 'Shard' by its host and port.
findShard :: Pipe -> String -> Int -> IO ObjectId
findShard pipe host port = do
    let query = select sel "containers"
    result <- access pipe master "encounter" $ findOne query
    case result of
        Left _  -> createShard
        Right x -> maybe createShard (lookup "_id") x

  where
    sel = [ "host" =: host, "port" =: port ]
    createShard = do
        id' <- genObjectId
        let query = insert "containers" (merge sel [ "_id" =: id', "capabilities" =: ["wardrobe" :: String] ])
        _ <- access pipe master "encounter" $ query
        return id'


updateHeartbeat :: Pipe -> ObjectId -> IO ()
updateHeartbeat pipe id' = do
    now <- getCurrentTime

    let query    = select [ "_id" =: id' ] "containers"
    let modifier = [ "$set" =: [ "heartbeat" =: now, "open" =: True, "active" =: True ] ]

    _ <- access pipe master "encounter" $ modify query modifier

    return ()

findOpenShard :: Pipe -> String -> IO (Maybe Shard)
findOpenShard pipe capability = do
    let query = select [ "active" =: True, "capabilities" =: capability ] "containers"
    result <- access pipe master "encounter" $ findOne query
    case result of
        Left _  -> return Nothing
        Right x -> maybe (return Nothing) (return . fromBSON) x
