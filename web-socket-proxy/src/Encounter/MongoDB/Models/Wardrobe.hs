module Encounter.MongoDB.Models.Wardrobe where

import Prelude hiding (lookup)
import Data.Bson (ObjectId, lookup)
import Control.Applicative
import Encounter.MongoDB.Types


data Wardrobe = Wardrobe
    { wardrobeId :: ObjectId
    , wardrobeShardId :: ObjectId
      -- ^ The shard where this wardrobe is running.

    , wardrobeEncounterId, wardrobeRevisionId :: ObjectId

    , wardrobePrivate :: Bool

    , wardrobePlayers :: [ String ]

    , wardrobeType :: String

    } deriving (Show)


instance FromBSON Wardrobe where
    fromBSON x = Wardrobe
        <$> lookup "_id" x
        <*> lookup "container" x
        <*> lookup "encounter" x
        <*> lookup "revision" x
        <*> lookup "private" x
        <*> lookup "authorizedAccounts" x
        <*> lookup "type" x
