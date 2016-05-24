{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Objects.Shard
    ( shardObjectType

      -- * Views
    , shardsView
    ) where


import Control.Applicative
import Control.Monad.State

import Avers
import Storage.Objects.Shard.Types

import Prelude



shardObjectType :: ObjectType Shard
shardObjectType = ObjectType
    { otType   = "shard"
    , otId     = ObjId <$> liftIO (newId 30)
    , otViews  = views
    }



------------------------------------------------------------------------------
-- Views

views :: [SomeView Shard]
views =
    [ SomeView shardsView
    ]


-- | All games.
shardsView :: View Shard Shard
shardsView = View
    { viewName              = "shards"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }
