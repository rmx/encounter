{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.PatchObject
    ( handlePatchObject
    ) where



import           Control.Applicative

import           Avers
import           Avers.TH

import           Snap (getQueryParam)

import           Types
import           Session
import           Http
import           Authorization

import           Prelude



data Request = Request
  { reqRevisionId :: RevId
    -- ^ The 'RevId' against which the client created the operations. This may
    -- be a bit behind if some other client submitted patches in parallel.

  , reqOperations :: [ Operation ]
    -- ^ The operations which the client wants to store in the database.
  }


data Response = Response
  { _resPreviousPatches :: ![Patch]
    -- ^ Patches which were already in the database. The submitted ops were
    -- rebased on top of these.

  , _resNumProcessedOperations :: !Int
    -- ^ The number of operations which were processed. This may be smaller
    -- than the number of submitted ops if the processing failed somewhere
    -- in the middle. The client can then decide what to do with those which
    -- were not accepted.

  , _resResultingPatches :: ![Patch]
    -- ^ Out of the submitted operations, these are the patches which were
    -- actually applied and stored in the database. This list may be shorter
    -- if some operations were dropped (because redundant or conflicting).
  }



handlePatchObject :: RequestHandler ()
handlePatchObject = do
    session <- requireSession

    novalidate <- maybe False (const True) <$> getQueryParam "novalidate"

    objId       <- asObjId =<< textParam "objId"
    Request{..} <- parseRequestBody

    res <- reqAvers $ do
        authorizePatch session objId

        applyObjectUpdates
            (BaseObjectId objId)
            reqRevisionId
            (sessionObjId session)
            reqOperations
            novalidate

    replyWith res $ \(previousPatches, numProcessedOperations, resultingPatches) -> do
        sendResponse $ Response
            previousPatches numProcessedOperations resultingPatches



$(deriveJSON (deriveJSONOptions "req") ''Request)
$(deriveJSON (deriveJSONOptions "_res") ''Response)
