module Encounter.MongoDB.Models.Spell where

import Prelude hiding (lookup)
import Data.Bson (ObjectId, lookup)
import Control.Applicative
import Encounter.MongoDB.Types


data Spell = Spell
    { spellId :: ObjectId
    , spellName :: String
    , spellDescription :: Maybe String
    , spellMaintainer :: ObjectId
    , spellIsLocked :: Bool
    , spellReplacedBy :: Maybe ObjectId
    , spellScript :: String
    }


instance FromBSON Spell where
    fromBSON x = Spell
        <$> lookup "_id" x
        <*> lookup "name" x
        <*> lookup "description" x
        <*> lookup "maintainer" x
        <*> lookup "locked" x
        <*> lookup "replacedBy" x
        <*> lookup "script" x
