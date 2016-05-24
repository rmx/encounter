module Encounter.Shard.Types where

import           Data.Aeson
import           Data.Time.Clock
import           Control.Concurrent.STM


type ClientMessageQueue = TQueue (String, UTCTime, Value)
type ServerMessageQueue = TQueue (String, Value)
type TargetAction       = ClientMessageQueue -> ServerMessageQueue -> IO ()
