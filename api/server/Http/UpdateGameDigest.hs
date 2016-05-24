{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Http.UpdateGameDigest
    ( httpUpdateGameDigest
    ) where


import           Control.Monad

import           Avers as Avers
import           Avers.TH

import           Types
import           Http
import           Storage.Objects.Game.Types



data Request = Request
  { reqStage    :: !Stage
  , reqDuration :: !Double
  , reqParties  :: ![Party]
  }



httpUpdateGameDigest :: RequestHandler ()
httpUpdateGameDigest = do
    gameId <- asObjId =<< textParam "gameId"
    Request{..} <- parseRequestBody

    void $ reqAvers $ do
        Snapshot{..} <- Avers.lookupLatestSnapshot (BaseObjectId gameId)
        Avers.applyObjectUpdates (BaseObjectId gameId) snapshotRevisionId rootObjId
            [ Set "stage"    (Just $ toJSON $ reqStage)
            , Set "duration" (Just $ toJSON $ reqDuration)
            , Set "parties"  (Just $ toJSON $ reqParties)
            ]
            False

    return ()



$(deriveJSON (deriveJSONOptions "req") ''Request)
