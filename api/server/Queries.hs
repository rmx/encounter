{-# LANGUAGE OverloadedStrings #-}

module Queries
    ( openGamesExp
    ) where


import           Data.Text   (Text)

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Database.RethinkDB     as R

import           Avers

import           Storage.ObjectTypes



mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"


openGamesExp :: Avers (Vector ObjId)
openGamesExp = fmap (V.map ObjId) $ runQueryCollect $
    R.Map mapId $
    R.OrderBy [R.Ascending "id"] $
    viewTable openGamesView
