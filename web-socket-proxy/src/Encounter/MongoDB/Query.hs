module Encounter.MongoDB.Query (findById, getAll) where

import Text.Read
import Data.Text
import Data.Maybe
import Database.MongoDB

import Encounter.MongoDB.Types


findById :: (FromBSON a) => Pipe -> Text -> String -> IO (Maybe a)
findById pipe collection id' =
    case readMaybe id' :: Maybe ObjectId of
        Nothing -> return Nothing
        Just id'' -> do
            let query = select [ "_id" =: id'' ] collection
            result <- access pipe master "encounter" $ findOne query
            case result of
                Left _  -> return Nothing
                Right x -> maybe (return Nothing) (return . fromBSON) x


getAll :: (FromBSON a) => Pipe -> Text -> IO [a]
getAll pipe collection = do
    let query = select [] collection
    result <- access pipe master "encounter" $ Database.MongoDB.find query
    case result of
        Left _  -> return []
        Right cursor -> do
            docs <- access pipe master "encounter" $ rest cursor
            case docs of
                Left _ -> return []
                Right x -> return (mapMaybe fromBSON x)
