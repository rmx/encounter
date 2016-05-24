{-# LANGUAGE OverloadedStrings #-}

module Storage.Objects.Image
    ( imageObjectType

    , Image(..)

      -- * Views
    , imagesView
    ) where


import Control.Applicative
import Control.Monad.State

import Avers
import Storage.Objects.Image.Types

import Prelude



imageObjectType :: ObjectType Image
imageObjectType = ObjectType
    { otType   = "image"
    , otId     = ObjId <$> liftIO (newId 30)
    , otViews  = views
    }



------------------------------------------------------------------------------
-- Views

views :: [SomeView Image]
views =
    [ SomeView imagesView
    ]



imagesView :: View Image Image
imagesView = View
    { viewName              = "images"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
