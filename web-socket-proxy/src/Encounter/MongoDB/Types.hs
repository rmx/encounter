module Encounter.MongoDB.Types where

import Data.Bson

class FromBSON a where
    fromBSON :: Document -> Maybe a
