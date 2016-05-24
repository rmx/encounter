{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Objects.Account
    ( accountObjectType

    , Account(..)

      -- * Views
    , accountsView
    ) where


import Control.Applicative
import Control.Monad.State

import Avers
import Storage.Objects.Account.Types

import Prelude



accountObjectType :: ObjectType Account
accountObjectType = ObjectType
    { otType   = "account"
    , otId     = ObjId <$> liftIO (newId 10) -- mkStdObjId
    , otViews  = views
    }



------------------------------------------------------------------------------
-- Views

views :: [SomeView Account]
views =
    [ -- SomeView accountsView
    ]


-- | All games.
accountsView :: View Account Account
accountsView = View
    { viewName              = "accounts"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
