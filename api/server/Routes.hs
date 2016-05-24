{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes (routes) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except

import qualified Data.ByteString.Lazy   as BL

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector            as V

import           Data.Aeson (Value(..), (.=), object)

import           Data.IORef
import qualified Data.OrdPSQ as PSQ
import           Data.Time
import qualified Data.Map as M

import qualified Database.RethinkDB    as R

import           Avers as Avers
import           Avers.TH

import           Snap

import           Types
import           Session
import           Http
import           Authorization

import           Http.ChangeSecret
import           Http.CreateAccount
import           Http.CreateGame
import           Http.CreateObject
import           Http.GetObject
import           Http.GetObjectValidation
import           Http.PatchObject
import           Http.UpdateGameDigest

import           Routes.Collections

import           Storage.Types
import           Storage.Resolver
import           Storage.ObjectTypes
import           Storage.Objects.Game.Types
import           Storage.Objects.Shard.Types


encounterContent :: ObjId -> Avers EncounterContent
encounterContent encId = do
    snapshot@Snapshot{..} <- Avers.lookupLatestSnapshot $ BaseObjectId encId
    enc@Encounter{..} <- parseValue snapshotContent
    bindings <- collectEncounterAssets snapshot enc
    return $ EncounterContent
        encId
        encounterGlue
        bindings
        (normalizedEncounterParties enc)
        encounterTerrainInstances
        encounterScoringFunction


gameContentHandler :: RequestHandler ()
gameContentHandler = do
    gameId <- asObjId =<< textParam "gameId"

    res <- reqAvers $ do
        game <- objectContent (BaseObjectId gameId)

        -- Refuse to hand out the content when the game is not in setup stage.
        when (gameStage game /= Blank) $
            strErr "gameContentHandler: invalid stage"

        case referenceObjectId (gameEncounter game) of
            Nothing    -> strErr "Game does not reference an encounter"
            Just objId -> encounterContent objId

    replyWith res sendResponse


------------------------------------------------------------------------------
-- GET /games/:gameId/shardAddress

gameShardAddressHandler :: RequestHandler ()
gameShardAddressHandler = do
    gameId <- asObjId =<< textParam "gameId"

    res <- rhAvers $ do
        game <- objectContent (BaseObjectId gameId)
        shard <- objectContent (BaseObjectId $ gameShardId game)
        return $ shardAddress shard

    sendResponse res



getEncounterContent :: RequestHandler ()
getEncounterContent = do
    objId <- asObjId =<< textParam "objId"
    res   <- reqAvers $ encounterContent objId

    replyWith res sendResponse


releasedEncountersHandler :: RequestHandler ()
releasedEncountersHandler = do
    let predicate :: R.Exp R.Object -> R.Exp Bool
        predicate = \x -> R.Eq
            (R.GetField "type" x :: R.Exp Text)
            (R.lift ("encounter"::Text))

    res <- reqAvers $ runQueryCollect $
        R.Map mapId $
        R.OrderBy [R.Descending "createdAt", R.Ascending "id"] $
        R.Filter isNotDeleted $
        R.Filter predicate objectsTable :: RequestHandler (Either AversError (V.Vector Text))

    replyWith res sendResponse

recommendationsHandler :: RequestHandler ()
recommendationsHandler = releasedEncountersHandler

newReleasesHandler :: RequestHandler ()
newReleasesHandler = releasedEncountersHandler



------------------------------------------------------------------------------
-- POST /surveys
--
-- TODO: Authorization.

data CreateSurveyRequest = CreateSurveyRequest
    { csrSurvey :: !Value
    }

$(deriveEncoding (deriveJSONOptions "csr") ''CreateSurveyRequest)

createSurveyHandler :: RequestHandler ()
createSurveyHandler = do
    CreateSurveyRequest{..} <- parseRequestBody

    rhAvers $ do
        let table = R.Table Nothing $ R.lift ("surveys" :: Text)
        let (R.Object content) = R.toDatum csrSurvey

        _ <- runQuery $ R.InsertObject R.CRError table content
        -- checkWriteResponse $ Just $ R.Object resp
        return ()

    sendResponse $ object []



------------------------------------------------------------------------------
-- POST /peers

data RegisterPeerRequest = RegisterPeerRequest
    { rprObjId  :: !ObjId
    , rprPeerId :: !PeerId
    , rprDevice :: !PeerDevice
    }

$(deriveEncoding (deriveJSONOptions "rpr") ''RegisterPeerRequest)

registerPeerHandler :: RequestHandler ()
registerPeerHandler = do
    RegisterPeerRequest{..} <- parseRequestBody

    ref <- gets _peerRegistry
    liftIO $ registerPeer ref rprObjId rprPeerId rprDevice

    sendResponse $ object []



------------------------------------------------------------------------------
-- GET /peers/:objId

getPeersHandler :: RequestHandler ()
getPeersHandler = do
    objId <- asObjId =<< textParam "objId"

    ref <- gets _peerRegistry
    mbPeers <- liftIO $ do
        reg <- readIORef ref
        return $ M.lookup objId reg

    case mbPeers of
        Nothing -> sendResponse $ Array V.empty
        Just peers -> sendResponse peers



------------------------------------------------------------------------------
-- POST /shards
--
-- TODO: Authorization.

data CreateShardRequest = CreateShardRequest
    { csrVersion :: !Text
    , csrAddress :: !(Text, Int)
    }

$(deriveEncoding (deriveJSONOptions "csr") ''CreateShardRequest)

createShardHandler :: RequestHandler ()
createShardHandler = do
    CreateShardRequest{..} <- parseRequestBody

    objId <- rhAvers $ do
        let content = Shard csrVersion csrAddress
        Avers.createObject shardObjectType rootObjId content

    sendResponse $ object [ "shardId" .= objId ]



------------------------------------------------------------------------------
-- GET /shards/:objId/health
--
-- TODO: Authorization.

getShardHealthHandler :: RequestHandler ()
getShardHealthHandler = do
    objId <- asObjId =<< textParam "objId"
    ref   <- gets _shardRegistry
    reg   <- liftIO $ readIORef ref

    case PSQ.lookup objId reg of
        Nothing -> failWith NotFound
        Just (load, _) -> sendResponse $ object [ "load" .= load ]



------------------------------------------------------------------------------
-- PUT /shards/:objId/health
--
-- TODO: Authorization.

data UpdateShardHealthRequest = UpdateShardHealthRequest
    { ushrLoad :: !Int
    }

$(deriveEncoding (deriveJSONOptions "ushr") ''UpdateShardHealthRequest)

updateShardHealthHandler :: RequestHandler ()
updateShardHealthHandler = do
    UpdateShardHealthRequest{..} <- parseRequestBody
    objId <- asObjId =<< textParam "objId"

    -- Verify that the object is a shard.
    void $ rhAvers $ do
        obj <- lookupObject objId
        when (objectType obj /= "shard") $
            throwError $ ObjectNotFound objId

    ref <- gets _shardRegistry
    now <- liftIO $ getCurrentTime
    liftIO $ atomicModifyIORef' ref $ \reg ->
        (PSQ.insert objId ushrLoad now reg, ())

    sendResponse $ object []



------------------------------------------------------------------------------
-- Sessions

createSession :: RequestHandler ()
createSession = do
    csr <- parseRequestBody
    createLocalSession csr


getSession :: RequestHandler ()
getSession = do
    cookie <- getSessionCookie
    case cookie of
        Nothing -> modifyResponse $ setResponseCode 404
        Just Cookie{..} -> do
            sessionDoc <- reqAvers $ lookupSession (SessionId $ T.decodeUtf8 cookieValue)
            case sessionDoc of
                Left _ -> modifyResponse $ setResponseCode 404
                Right session -> do
                    touchSessionCookie
                    sendResponse $ ClientSession (unSessionId $ sessionId session) (unObjId $ sessionObjId session)


deleteSession :: RequestHandler ()
deleteSession = do
    cookie <- getSessionCookie
    expireSessionCookie

    case cookie of
        Nothing -> return ()
        Just Cookie{..} -> void $ reqAvers $ dropSession (SessionId $ T.decodeUtf8 cookieValue)



------------------------------------------------------------------------------
-- Blobs

blobFromRequestBody :: RequestHandler Blob
blobFromRequestBody = do
    session <- requireSession

    mbContentType <- getHeader "Content-Type" <$> getRequest
    case mbContentType of
        Nothing -> failWith $ BadRequest "Content-Type header is missing"
        Just contentType -> do
            body <- readRequestBody (100 * 1024 * 1024)
            ret <- reqAvers $ do
                authorizeBlobCreate session
                Avers.createBlob body (T.decodeUtf8 contentType)

            case ret of
                Left e -> failWith $ BadRequest (T.pack $ show e)
                Right blob -> return blob


httpCreateBlob :: RequestHandler ()
httpCreateBlob = do
    blob <- blobFromRequestBody
    sendResponse blob


getBlob :: RequestHandler ()
getBlob = do
    bId <- BlobId <$> textParam "blobId"
    res <- reqAvers $ Avers.lookupBlob bId

    replyWith res $ \Blob{..} -> do
        modifyResponse $ setContentType (T.encodeUtf8 blobContentType)
        content <- liftIO $ BL.readFile $ "blobs/" ++ (T.unpack $ unBlobId blobId)
        writeLBS content


httpCreateImage :: RequestHandler ()
httpCreateImage = do
    mbRelatedId <- fmap ObjId <$> optionalTextParam "relatedId"
    blob        <- blobFromRequestBody

    session <- requireSession
    res <- reqAvers $ do
        let image = Image (blobId blob) mbRelatedId
        Avers.createObject imageObjectType (sessionObjId session) image

    replyWith res $ \objId -> sendResponse $ object [ "objId" .= objId ]



getPatchHandler :: RequestHandler ()
getPatchHandler = do
    objId <- asObjId =<< textParam "objId"
    revId <- asRevId =<< textParam "revId"

    res <- reqAvers $ lookupPatch (BaseObjectId objId) revId
    replyWith res $ \patch@Patch{..} -> do
        let etag = objRevETag "patch" patchObjectId patchRevisionId
        sendCachedResponse etag patch


latestReleaseHandler :: RequestHandler ()
latestReleaseHandler = do
    objId <- asObjId =<< textParam "objId"
    res <- reqAvers $ Avers.lookupLatestRelease objId
    replyWith res sendResponse


objAndRevIds :: RequestHandler (ObjId, RevId)
objAndRevIds = (,) <$> (textParam "objId" >>= asObjId) <*> (textParam "revId" >>= asRevId)


lookupReleaseHandler :: RequestHandler ()
lookupReleaseHandler = do
    (objId, revId) <- objAndRevIds
    res <- reqAvers $ Avers.lookupRelease objId revId
    replyWith res sendResponse


createReleaseHandler :: RequestHandler ()
createReleaseHandler = do
    session <- requireSession

    (objId, revId) <- objAndRevIds
    res <- reqAvers $ do
        authorizeCreateRelease session
        Avers.createRelease objId revId
    replyWith res sendResponse


deleteObjectHandler :: RequestHandler ()
deleteObjectHandler = do
    session <- requireSession
    objId   <- asObjId =<< textParam "objId"

    res <- reqAvers $ do
        authorizeObjectDelete session objId
        Avers.deleteObject objId
    replyWith res sendResponse



routes :: [Route]
routes =
    [ ( POST,   "objects",                         handleCreateObject)
    , ( GET,    "objects/:objId",                  handleGetObject)
    , ( GET,    "objects/:objId/patches/:revId",   getPatchHandler)

    , ( GET,    "objects/:objId/release/latest",   latestReleaseHandler)
    , ( GET,    "objects/:objId/release/:revId",   lookupReleaseHandler)
    , ( POST,   "objects/:objId/release/:revId",   createReleaseHandler)

    , ( GET,    "objects/:objId/validation",       handleGetObjectValidation)
    , ( PATCH,  "objects/:objId",                  handlePatchObject)
    , ( DELETE, "objects/:objId",                  deleteObjectHandler)

    , ( POST,   "session",                         createSession)
    , ( GET,    "session",                         getSession)
    , ( DELETE, "session",                         deleteSession)

    , ( POST,   "account",                         createAccountHandler)
    , ( POST,   "secret",                          handleChangeSecret)

    , ( POST,   "games",                           createGameHandler)

    , ( POST,   "surveys",                         createSurveyHandler)

    , ( POST,   "peers",                           registerPeerHandler)
    , ( GET,    "peers/:objId",                    getPeersHandler)

    , ( PUT,    "games/:gameId/digest",            httpUpdateGameDigest)
    , ( GET,    "games/:gameId/encounterContent",  gameContentHandler)
    , ( GET,    "games/:gameId/shardAddress",      gameShardAddressHandler)
    , ( GET,    "encounters/:objId/content",       getEncounterContent)

    , ( POST,   "blobs",                           httpCreateBlob)
    , ( GET,    "blobs/:blobId",                   getBlob)

    , ( POST,   "images",                          httpCreateImage)
    , ( POST,   "images/:relatedId",               httpCreateImage)

    , ( POST,   "shards",                          createShardHandler)
    , ( GET,    "shards/:objId/health",            getShardHealthHandler)
    , ( PUT,    "shards/:objId/health",            updateShardHealthHandler)

    , ( GET,    "collection/featuredEncounters",   recommendationsHandler)
    , ( GET,    "collection/recommendations",      recommendationsHandler)
    , ( GET,    "collection/newReleases",          newReleasesHandler)
    , ( GET,    "collection/releasedEncounters",   releasedEncountersHandler)

    , gamesUsingEncounterCollectionRoute
    , imagesRelatedToCollectionRoute
    , openGamesCollectionRoute
    , encounterLeaderboardCollectionRoute
    , activeShardsCollectionRoute

    , objectsOfTypeCollectionRoute "accounts"        accountObjectType
    , objectsOfTypeCollectionRoute "encounters"      encounterObjectType
    , objectsOfTypeCollectionRoute "icons"           iconObjectType
    , objectsOfTypeCollectionRoute "models"          modelObjectType
    , objectsOfTypeCollectionRoute "skyboxes"        skyboxObjectType
    , objectsOfTypeCollectionRoute "sounds"          soundObjectType
    , objectsOfTypeCollectionRoute "tiles"           tileObjectType
    , objectsOfTypeCollectionRoute "particleeffects" particleeffectObjectType
    ]
