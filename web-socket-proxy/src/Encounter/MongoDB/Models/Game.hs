module Encounter.MongoDB.Models.Game where

import Prelude hiding (lookup)
import Data.Bson
import Control.Applicative
import Encounter.MongoDB.Types
import Database.MongoDB hiding (host)
import Control.Monad
import Data.Typeable


data GamePlayer = GamePlayer
    { gamePlayerId :: String
    , gamePlayerRoleId :: String
    } deriving (Show, Eq, Typeable)

instance FromBSON GamePlayer where
    fromBSON x = GamePlayer
        <$> lookup "_id" x
        <*> lookup "roleId" x

instance Val GamePlayer where
    val x = Doc
        [ "_id"    =: gamePlayerId x
        , "roleId" =: gamePlayerRoleId x
        ]

    cast' (Doc x) = fromBSON x
    cast' _ = Nothing


data GameTeam = GameTeam
    { gameTeamId :: ObjectId
    , gameTeamScore :: Double
    , gameTeamPlayers :: [GamePlayer]
    } deriving (Show, Eq, Typeable)

instance FromBSON GameTeam where
    fromBSON x = GameTeam
        <$> lookup "_id" x
        <*> lookup "score" x
        <*> lookup "members" x

instance Val GameTeam where
    val x = Doc
        [ "_id"     =: gameTeamId x
        , "score"   =: gameTeamScore x
        , "players" =: gameTeamPlayers x
        ]

    cast' (Doc x) = fromBSON x
    cast' _ = Nothing


data Game = Game
    { gameId :: ObjectId
    , gameShardId :: ObjectId
    , gameToken :: String
    , gameRevisionId :: ObjectId
    , gameEncounterId :: ObjectId
    , gamePlayers :: [GamePlayer]
    , gameTeams :: [GameTeam]
    , gameType :: String
    } deriving (Show, Typeable, Eq)


instance FromBSON Game where
    fromBSON x = Game
        <$> lookup "_id" x
        <*> lookup "container" x
        <*> lookup "token" x
        <*> lookup "revision" x
        <*> lookup "encounter" x
        <*> lookup "players" x
        <*> lookup "teams" x
        <*> lookup "type" x

instance Val Game where
    val x = Doc
        [ "_id"     =: gameId x
        , "container"   =: gameShardId x
        , "token"   =: gameToken x
        , "revision"   =: gameRevisionId x
        , "encounter"   =: gameEncounterId x
        , "players"   =: gamePlayers x
        , "teams"   =: gameTeams x
        , "type"   =: gameType x
        ]

    cast' (Doc x) = fromBSON x
    cast' _ = Nothing


saveGame :: Pipe -> Game -> IO ()
saveGame pipe game = do
    let (Doc doc) = val game
    void $ access pipe master "encounter" $ insert "games" doc
