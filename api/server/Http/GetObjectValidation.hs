{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.GetObjectValidation
    ( handleGetObjectValidation
    ) where


import           Data.Text (Text)
import qualified Data.Text as T

import           Avers as Avers
import           Avers.TH

import           Types
import           Http



data Response = Response
  { _resOk    :: Bool
  , _resError :: Maybe Text
  }



handleGetObjectValidation :: RequestHandler ()
handleGetObjectValidation = do
    objId <- asObjId =<< textParam "objId"

    res <- reqAvers $ do
        Object{..}   <- Avers.lookupObject objId
        Snapshot{..} <- Avers.lookupLatestSnapshot (BaseObjectId objId)

        validateObject objectType snapshotContent

    case res of
        Left  e -> sendResponse $ Response False (Just $ T.pack $ show e)
        Right _ -> sendResponse $ Response True Nothing


$(deriveJSON (deriveJSONOptions "_res") ''Response)
