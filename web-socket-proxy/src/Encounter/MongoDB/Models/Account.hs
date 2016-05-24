module Encounter.MongoDB.Models.Account where

import Prelude hiding (lookup)
import Data.Bson (lookup)
import Control.Applicative
import Encounter.MongoDB.Types


data Account = Account
    { accountId :: String
    , accountEmail :: String
    , accountNick :: String
    }


instance FromBSON Account where
    fromBSON x = Account
        <$> lookup "_id" x
        <*> lookup "email" x
        <*> lookup "nick" x
