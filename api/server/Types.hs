{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where


import           Control.Exception (throw)
import           Control.Exception (Exception)
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Data.Monoid
import           Data.Typeable
import           Data.Aeson (encode)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.IORef
import           Data.OrdPSQ
import           Data.Time
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List
import           Data.ByteString (ByteString)


import           Snap

import           Avers
import           Avers.Types (parseObjectId)
import           Avers.TH



type ShardRegistry = IORef (OrdPSQ ObjId Int UTCTime)


data PeerDevice = PeerDevice
    { pdUserAgent :: !Text
    , pdPlatform  :: !Text
    } deriving (Show)

deriveEncoding (deriveJSONOptions "pd") ''PeerDevice

type PeerId = Text -- The PeerJS id.

data Peer = Peer
    { peerId        :: !PeerId
    , peerUpdatedAt :: !UTCTime
    , peerDevice    :: !PeerDevice
    } deriving (Show)

deriveEncoding (deriveJSONOptions "peer") ''Peer


type PeerRegistry  = IORef (Map ObjId [Peer])
-- ^ The values are ordered by the updated at time, with the most recent being
-- at the head.

registerPeer :: PeerRegistry -> ObjId -> PeerId -> PeerDevice -> IO ()
registerPeer pr objId pId device = do
    now <- getCurrentTime
    atomicModifyIORef' pr $ \reg ->
        (M.insertWith (combinePeerLists now) objId [Peer pId now device] reg, ())
  where
    combinePeerLists now new old =
        case Data.List.find (\Peer{..} -> peerId == pId) old of
            Nothing -> old ++ new
            Just _  -> Prelude.map (\p -> if peerId p == pId then p { peerUpdatedAt = now, peerDevice = device } else p) old


data AppState = AppState
    { _aversHandle    :: !Avers.Handle

    , _shardRegistry :: !ShardRegistry
      -- ^ All active shards. The priority is their load. When selecting
      -- a shard to assign to a game, the one with the lowest priority (load)
      -- is taken.

    , _peerRegistry :: !PeerRegistry
      -- ^ Manages the mapping from PeerId to ObjId.
    }

type RequestHandler = Handler AppState AppState


-- | All exceptions which may be safely thrown within a 'RequestHandler'. They
-- are caught by the snaplet and converted into the appropriate response code.

data RequestHandlerException

    = BadRequest !Text
      -- ^ 400 -- The client has submitted a malformed request.

    | Unauthorized
      -- ^ 401 -- Only sent in response to 'POST /session'. It means the
      -- submitted credentials were invalid.

    | Forbidden
      -- ^ 403 -- The request can not be fulfilled because the user does not
      -- have the necessary permissions.

    | NotFound
      -- ^ 404 -- The resource has not been found.

    | InternalServerError !Text
      -- ^ 500 -- Something has gone bad inside the server. It is not the
      -- client's fault. It can try sending the same request again after
      -- a short delay.

    deriving (Show, Typeable)

instance Exception RequestHandlerException


failWith :: RequestHandlerException -> RequestHandler a
failWith = throw


sendErrorResponse :: Int -> Text -> RequestHandler ()
sendErrorResponse status text = do
    liftIO $ print $ "error " <> (T.pack $ show status) <> ": " <> text
    modifyResponse $ setResponseCode status
    writeText $ "{\"error\":\"" <> text <> "\"}"


sendResponse :: (ToJSON a) => a -> RequestHandler ()
sendResponse body = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode body

sendCachedResponse :: (ToJSON a) => ByteString -> a -> RequestHandler ()
sendCachedResponse etag body = do
    mbLastETag <- getsRequest $ getHeader "If-None-Match"
    if mbLastETag == Just (etagVersion <> ":" <> etag)
        then modifyResponse $ setResponseCode 304
        else do
            cacheIndefinitely
            modifyResponse $ setHeader "ETag" $ etagVersion <> ":" <> etag
            sendResponse body

etagVersion :: ByteString
etagVersion = "v0"

objRevETag :: ByteString -> ObjectId -> RevId -> ByteString
objRevETag prefix objId (RevId revId) =
    prefix <> "/" <> T.encodeUtf8 (toPk objId) <> "@" <> T.encodeUtf8 (T.pack $ show revId)

cacheIndefinitely :: RequestHandler ()
cacheIndefinitely = modifyResponse $ setHeader "Cache-Control" $
    "no-cache, public, max-age=63072000"


-- | Run an Avers action within the request handler, using
-- the default Avers configuration.
reqAvers :: Avers a -> RequestHandler (Either AversError a)
reqAvers m = do
    h <- gets _aversHandle
    liftIO $ evalAvers h m


rhAvers :: Avers a -> RequestHandler a
rhAvers action = reqAvers action >>= aversResult


aversResult :: Either AversError a -> RequestHandler a
aversResult res = case res of
    Left e -> case e of
        DatabaseError detail                  -> failWith $ InternalServerError detail
        NotAuthorized                         -> failWith $ Unauthorized
        DocumentNotFound _                    -> failWith $ NotFound
        UnknownObjectType detail              -> failWith $ InternalServerError detail
        ObjectNotFound _                      -> failWith $ NotFound
        ParseError _ detail                   -> failWith $ InternalServerError detail
        PatchError (UnknownPatchError detail) -> failWith $ InternalServerError detail
        AversError detail                     -> failWith $ InternalServerError detail
        InternalError ie                      -> aversResult (Left ie)
    Right r -> return r


replyWith :: Either AversError a -> (a -> RequestHandler ()) -> RequestHandler ()
replyWith res action = aversResult res >>= action


asObjId :: Text -> RequestHandler ObjId
asObjId text = return $ ObjId text

asObjectId :: Text -> RequestHandler ObjectId
asObjectId text = case parseObjectId text of
    Nothing -> failWith $ BadRequest $ "Not an ObjectId: " <> text
    Just v  -> return v


asRevId :: Text -> RequestHandler RevId
asRevId text = return $ RevId (read $ T.unpack text)


type Route = (Method, ByteString, RequestHandler ())
