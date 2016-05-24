{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.GetObject
    ( handleGetObject
    ) where


import           Data.Text (Text)
import           Data.Aeson (Value)
import           Data.Time.Clock

import           Avers as Avers
import           Avers.TH

import           Types
import           Http


data Response = Response
  { _resId         :: ObjId
  , _resType       :: Text
  , _resCreatedAt  :: UTCTime
  , _resCreatedBy  :: ObjId
  , _resRevisionId :: RevId
  , _resContent    :: Value
  }



handleGetObject :: RequestHandler ()
handleGetObject = do
    objId <- asObjId =<< textParam "objId"

    res <- reqAvers $ do
        object   <- Avers.lookupObject objId
        snapshot <- Avers.lookupLatestSnapshot (BaseObjectId objId)

        return (object, snapshot)

    replyWith res $ \(Object{..}, Snapshot{..}) -> do
        let etag = objRevETag "object" (BaseObjectId objectId) snapshotRevisionId
        sendCachedResponse etag $ Response
            objId
            objectType
            objectCreatedAt
            objectCreatedBy
            snapshotRevisionId
            snapshotContent



$(deriveJSON (deriveJSONOptions "_res") ''Response)
