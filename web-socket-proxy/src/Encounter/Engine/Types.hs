module Encounter.Engine.Types where


import           Data.Fixed


data Vector3 a = Vector3 a a a
    deriving (Show)

data Id = Id { _id :: String } deriving (Eq, Ord, Show)

-- Time is counted in SI seconds, with an precision of e-3 (milliseconds).
type Time = Fixed E3

data Terrain = Terrain
data Objective = Objective
data Achievement = Achievement


data Player = Player
    { _playerId :: Id
    , _displayName :: String
    , _gravatarUrl :: String
    , _controlledObjectId :: Id
    , _playerRoleId :: Maybe Id
    } deriving (Show)

data Team = Team
    { _score :: Double
    , _members :: [ Player ]
    }

data Position = Position
    { _terrainId :: Id
    , _coordinates :: Vector3 Double
    , _orientation :: Double
    } deriving (Show)

data Model = Model
    { _modelId :: Id
    , _boundingRadius :: Double
    }

data Tile = Tile
    { _tileId :: Id
    }

data Sound = Sound
    { _soundId :: Id
    }

data WorldObject = WorldObject
    { _worldObjectId :: Id
    , _position :: Position
    }

data Event = Event
    { _eventName :: String
    }

data SpellInfo = SpellInfo
    { _spellInfoId :: String
    }

data SpellTarget
    = LocationTarget
    | WorldObjectTarget
    deriving (Show)
