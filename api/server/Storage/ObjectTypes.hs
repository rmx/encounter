{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|

This modules defines Avers ObjectTypes of objects that are stored in the database.
Not all specs are for top-level objects, some types can only appear as
resources within an encounter.

-}

module Storage.ObjectTypes
    ( module Storage.Objects.Account
    , module Storage.Objects.Encounter
    , module Storage.Objects.Game
    , module Storage.Objects.Image
    , module Storage.Objects.Shard

    , iconObjectType
    , modelObjectType
    , particleeffectObjectType
    , skyboxObjectType
    , soundObjectType
    , tileObjectType
    ) where


import           Control.Applicative
import           Control.Monad.State

import           Avers

import           Storage.Types

import           Storage.Objects.Account
import           Storage.Objects.Encounter
import           Storage.Objects.Game
import           Storage.Objects.Image
import           Storage.Objects.Shard

import           Prelude



mkObjId :: Int -> Avers ObjId
mkObjId len = ObjId <$> liftIO (newId len)

mkStdObjId :: Avers ObjId
mkStdObjId = mkObjId 10



modelObjectType :: ObjectType Model
modelObjectType = ObjectType
    { otType   = "model"
    , otId     = mkStdObjId
    , otViews  = []
    }

particleeffectObjectType :: ObjectType ParticleEffect
particleeffectObjectType = ObjectType
    { otType   = "particleeffect"
    , otId     = mkStdObjId
    , otViews  = []
    }

tileObjectType :: ObjectType Tile
tileObjectType = ObjectType
    { otType   = "tile"
    , otId     = mkStdObjId
    , otViews  = []
    }

soundObjectType :: ObjectType Sound
soundObjectType = ObjectType
    { otType   = "sound"
    , otId     = mkStdObjId
    , otViews  = []
    }

skyboxObjectType :: ObjectType Skybox
skyboxObjectType = ObjectType
    { otType   = "skybox"
    , otId     = mkStdObjId
    , otViews  = []
    }

iconObjectType :: ObjectType Icon
iconObjectType = ObjectType
    { otType   = "icon"
    , otId     = mkStdObjId
    , otViews  = []
    }
