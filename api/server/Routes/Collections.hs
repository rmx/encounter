{-# LANGUAGE OverloadedStrings #-}

module Routes.Collections
    ( gamesUsingEncounterCollectionRoute
    , imagesRelatedToCollectionRoute
    , objectsOfTypeCollectionRoute
    , openGamesCollectionRoute
    , encounterLeaderboardCollectionRoute

    , activeShardsCollectionRoute

    , mapId
    , isNotDeleted
    ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding     as T
import qualified Data.Vector            as V
import           Data.IORef
import qualified Data.OrdPSQ as PSQ

import           Snap

import           Avers as Avers

import qualified Database.RethinkDB     as R

import           Types
import           Session
import           Http
import           Queries

import           Storage.ObjectTypes


mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"

isNotDeleted :: R.Exp R.Object -> R.Exp Bool
isNotDeleted x = R.Any
    [ R.Not $ R.HasFields ["deleted"] x
    , R.Eq
        (R.GetField "deleted" x :: R.Exp Bool)
        (R.lift False)
    ]



-----------------------------------------------------------------------------
-- Collection: games/usingEncounter/:encounterId

gamesUsingEncounterCollectionRoute :: Route
gamesUsingEncounterCollectionRoute =
    ( GET
    , "collection/games/usingEncounter/:encounterId"
    , gamesUsingEncounterCollectionHandler
    )

gamesUsingEncounterCollectionHandler :: RequestHandler ()
gamesUsingEncounterCollectionHandler = do
    void $ requireSession

    encounterId <- textParam "encounterId"

    objIds <- reqAvers $ do
        let predicate :: R.Exp R.Object -> R.Exp Bool
            predicate = \x -> R.Eq
                (R.GetField "objectId" (R.GetField "encounter" x :: R.Exp R.Object) :: R.Exp Text)
                (R.lift encounterId)

        runQueryCollect $
            R.Map mapId $
            R.OrderBy [R.Descending "id"] $
            R.Filter predicate $
            viewTable gamesView

    case objIds :: Either AversError (V.Vector Text) of
        Right x -> sendResponse $ map ObjId $ V.toList x
        _       -> failWith NotFound



-----------------------------------------------------------------------------
-- Collection: images/relatedTo/:objId

imagesRelatedToCollectionRoute :: Route
imagesRelatedToCollectionRoute =
    ( GET
    , "collection/images/relatedTo/:objId"
    , imagesRelatedToCollectionHandler
    )

imagesRelatedToCollectionHandler :: RequestHandler ()
imagesRelatedToCollectionHandler = do
    void $ requireSession

    objId <- textParam "objId"

    objIds <- reqAvers $ do
        let predicate = \x -> R.Eq
                (R.GetField "relatedId" (x::R.Exp R.Object) :: R.Exp Text)
                (R.lift objId)

        runQueryCollect $
            R.Map mapId $
            R.OrderBy [R.Descending "id"] $
            R.Filter predicate $
            viewTable imagesView

    case objIds :: Either AversError (V.Vector Text) of
        Right x -> sendResponse $ map ObjId $ V.toList x
        _       -> failWith NotFound



-----------------------------------------------------------------------------
-- Collection: :objectType

objectsOfTypeCollectionRoute :: Text -> ObjectType a -> Route
objectsOfTypeCollectionRoute name objType =
    ( GET
    , "collection/" <> T.encodeUtf8 name
    , objectsOfTypeCollectionHandler objType
    )

objectsOfTypeCollectionHandler :: ObjectType a -> RequestHandler ()
objectsOfTypeCollectionHandler objType = do
    res <- reqAvers $ Avers.objectsOfType objType
    replyWith res (sendResponse . V.toList)



-----------------------------------------------------------------------------
-- Collection: openGames

openGamesCollectionRoute :: Route
openGamesCollectionRoute =
    ( GET
    , "collection/openGames"
    , openGamesCollectionHandler
    )

openGamesCollectionHandler :: RequestHandler ()
openGamesCollectionHandler = do
    void $ requireSession

    objIds <- reqAvers openGamesExp
    replyWith objIds sendResponse



-----------------------------------------------------------------------------
-- Collection: encounterLeaderboard

encounterLeaderboardCollectionRoute :: Route
encounterLeaderboardCollectionRoute =
    ( GET
    , "collection/encounter/:encounterId/leaderboard"
    , encounterLeaderboardCollectionHandler
    )

encounterLeaderboardCollectionHandler :: RequestHandler ()
encounterLeaderboardCollectionHandler = do
    void $ requireSession
    encounterId <- textParam "encounterId"

    objIds <- reqAvers $ do
        runQueryCollect $
            R.Limit 20 $
            R.Map mapId $
            R.OrderByIndexed (R.Ascending "leaderboard") $
            R.BetweenIndexed "leaderboard"
                ( R.Open $ R.toDatum [R.toDatum encounterId]
                , R.Open $ R.toDatum [R.toDatum encounterId, R.toDatum (1::Int)]
                ) $
            viewTable finishedGamesView

    case objIds :: Either AversError (V.Vector Text) of
        Right x -> sendResponse $ map ObjId $ V.toList x
        _       -> failWith NotFound



-----------------------------------------------------------------------------
-- Collection: activeShardsCollectionRoute

activeShardsCollectionRoute :: Route
activeShardsCollectionRoute =
    ( GET
    , "collection/activeShards"
    , activeShardsCollectionHandler
    )

activeShardsCollectionHandler :: RequestHandler ()
activeShardsCollectionHandler = do
    void $ requireSession

    ref <- gets _shardRegistry
    reg <- liftIO $ readIORef ref

    sendResponse $ PSQ.keys reg
