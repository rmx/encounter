module Encounter.MongoDB.Pipe (openMongoDB, Pipe) where

import Data.Maybe
import System.Environment
import Database.MongoDB


openMongoDB :: IO Pipe
openMongoDB = do
    env <- lookupEnv "MONGO"
    runIOE $ connect $ host $ fromMaybe "127.0.0.1" env
