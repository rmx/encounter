{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Session
    ( getSessionCookie
    , touchSessionCookie
    , expireSessionCookie

    , ClientSession(..)

    , createLocalSession

    , currentSession
    , requireSession
    , sessionAccount
    , authenticatedAccountId
    ) where



import           Control.Monad
import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)

import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Data.Time

import           Snap

import           Avers
import           Avers.TH

import           Types
import           Storage.Objects.Account



-- The record that is sent to the client.
data ClientSession = ClientSession
    { clientSessionId        :: !Text
    , clientSessionAccountId :: !Text
    }

$(deriveJSON (deriveJSONOptions "clientSession") ''ClientSession)


describeClientSession :: Session -> ClientSession
describeClientSession Session{..} = ClientSession
    { clientSessionId        = toPk sessionId
    , clientSessionAccountId = toPk sessionObjId
    }



data CreateSessionRequest = CreateSessionRequest
  { csrLogin  :: !SecretId
  , csrSecret :: Text
  }

$(deriveJSON (deriveJSONOptions "csr") ''CreateSessionRequest)



sessionCookieName :: ByteString
sessionCookieName = "session"

-- | The session expires two years after it is created.
sessionExpirationTime :: Int
sessionExpirationTime = 2 * 365 * 24 * 60 * 60


currentSessionId :: RequestHandler SessionId
currentSessionId = do
    cookie <- getCookie sessionCookieName
    case cookie of
        Nothing -> failWith Unauthorized
        Just x  -> return $ SessionId $ E.decodeUtf8 $ cookieValue x


currentSession :: RequestHandler (Maybe Session)
currentSession = do
    sessId <- currentSessionId
    res <- reqAvers $ lookupSession sessId
    return $ case res of
        Left _ -> Nothing
        Right session -> Just session

requireSession :: RequestHandler Session
requireSession = do
    mbSession <- currentSession
    maybe (failWith Unauthorized) (return) mbSession

authenticatedAccountId :: RequestHandler Text
authenticatedAccountId = do
    Session{..} <- requireSession
    return $ toPk sessionObjId

sessionAccount :: RequestHandler Account
sessionAccount = do
    accountId <- authenticatedAccountId
    res <- reqAvers $ objectContent $ BaseObjectId $ ObjId accountId
    either (const $ failWith $ InternalServerError "sessionAccount") (return . id) res


createSessionFor :: SecretId -> RequestHandler ()
createSessionFor secId = do
    now    <- liftIO $ getCurrentTime
    sessId <- liftIO $ newId 80
    isSecure <- rqIsSecure <$> getRequest

    let expiresAt = addUTCTime (fromIntegral sessionExpirationTime) now
    let cookie = Cookie { cookieName     = sessionCookieName
                        , cookieValue    = E.encodeUtf8 sessId
                        , cookieExpires  = Just expiresAt
                        , cookieDomain   = Nothing
                        , cookiePath     = Just "/"
                        , cookieSecure   = isSecure
                        , cookieHttpOnly = True
                        }

    modifyResponse $ addResponseCookie cookie

    let session = Session (SessionId sessId) (ObjId $ unSecretId secId) now now

    void $ reqAvers $ saveSession session
    sendResponse $ describeClientSession session


createLocalSession :: CreateSessionRequest -> RequestHandler ()
createLocalSession CreateSessionRequest{..} = do
    res <- reqAvers $ verifySecret csrLogin csrSecret
    case res of
        Left  _ -> failWith NotFound
        Right _ -> createSessionFor csrLogin


getSessionCookie :: RequestHandler (Maybe Cookie)
getSessionCookie = getCookie sessionCookieName


-- | If the request has a session cookie then update its expiration time.
touchSessionCookie :: RequestHandler ()
touchSessionCookie = do
    mbCookie <- getSessionCookie
    case mbCookie of
        Nothing -> return ()
        Just cookie -> do
            now <- liftIO $ getCurrentTime
            let expiresAt = addUTCTime (fromIntegral sessionExpirationTime) now

            modifyResponse $ addResponseCookie $
                cookie { cookieExpires = Just expiresAt }


expireSessionCookie :: RequestHandler ()
expireSessionCookie = expireCookie sessionCookieName Nothing
