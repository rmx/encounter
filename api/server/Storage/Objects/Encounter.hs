{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Objects.Encounter
    ( encounterObjectType

    , Encounter(..)

    , normalizedEncounterParties

      -- * Views
    , encountersView
    ) where


import Control.Applicative
import Control.Monad.State

import Avers
import Storage.Objects.Encounter.Types

import Prelude



encounterObjectType :: ObjectType Encounter
encounterObjectType = ObjectType
    { otType   = "encounter"
    , otId     = ObjId <$> liftIO (newId 10) -- mkStdObjId
    , otViews  = views
    }



------------------------------------------------------------------------------
-- Views

views :: [SomeView Encounter]
views =
    [ -- SomeView encountersView
    ]


-- | All games.
encountersView :: View Encounter Encounter
encountersView = View
    { viewName              = "encounters"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
