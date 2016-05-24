{-# LANGUAGE OverloadedStrings #-}

module Authorization where


import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Except

import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe

import           System.Environment

import           Avers as Avers

import           Prelude


-- | The result of an authorization query. The combinators are modelled losely
-- after PAM (with control flags)
data AuthzResult a
    = Continue a
      -- ^ The result is undecided. We con continue executing the following
      -- instructions.

    | Fail a
      -- ^ The result will be ultimately be Reject, but we continue executing
      -- following instructions.

    | Allow
      -- ^ The authorization query definitely succeeded. Any following
      -- instructions are skipped.

    | Reject
      -- ^ The authorization query definitely failed. Any following
      -- instructions are skipped.



data Authz a = Authz { runAuthz :: Avers (AuthzResult a) }


instance Functor Authz where
    fmap f m = Authz $ do
        res <- runAuthz m
        case res of
            Continue a -> return $ Continue $ f a
            Fail     a -> return $ Fail $ f a
            Allow      -> return Allow
            Reject     -> return Reject

instance Applicative Authz where
    pure = Authz . pure . Continue
    mf <*> m = Authz $ do
        fres <- runAuthz mf
        mres <- runAuthz m
        case (fres, mres) of
            (Continue f, Continue res) -> return $ Continue $ f res
            _                          -> return Reject

instance Monad Authz where
    return = Authz . return . Continue

    (Authz m) >>= f = Authz $ do
        res <- m
        case res of
            Continue a -> runAuthz $ f a
            Fail     a -> runAuthz $ f a
            Allow      -> return Allow
            Reject     -> return Reject


-- | An authorization module is an Avers action which returns a boolean
-- indicating whether it has succeeded or failed. Exceptions are treated
-- as failure.
type AuthzModule = Avers Bool



sufficient :: AuthzModule -> Authz ()
sufficient m = Authz $ do
    res <- m
    return $ if res then Allow else Continue ()


requisite :: AuthzModule -> Authz ()
requisite m = Authz $ do
    res <- m
    return $ if not res then Reject else Continue ()


required :: AuthzModule -> Authz ()
required m = Authz $ do
    res <- m
    return $ if not res then Fail () else Continue ()



runAuthorization :: Avers () -> Authz a -> Avers ()
runAuthorization def authz = do
    res <- runAuthz authz
    case res of
        Allow  -> return ()
        Reject -> throwError NotAuthorized
        Fail _ -> throwError NotAuthorized
        _      -> def



adminObjIds :: Avers [ObjId]
adminObjIds = liftIO $ do
    adminsString <- fromMaybe "" <$> (lookupEnv "ADMINS")
    return $ map (ObjId . T.pack) $ splitOn "," adminsString


-- | Anyone can create object encounters, admins can create any objects.
authorizeObjectCreate :: Session -> Text -> Avers ()
authorizeObjectCreate session objType = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin session
    sufficient $ return (objType == "encounter")


-- | Anyone can create new blobs.
authorizeBlobCreate :: Session -> Avers ()
authorizeBlobCreate _ = return ()


-- | Releases can only be created by admins (for the time being).
authorizeCreateRelease :: Session -> Avers ()
authorizeCreateRelease session = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin session


-- | Only admins can delete (and undelete) objects.
authorizeObjectDelete :: Session -> ObjId -> Avers ()
authorizeObjectDelete session _objId = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin session


-- | Admins can patch anything. The owner of an object can patch the object.
authorizePatch :: Session -> ObjId -> Avers ()
authorizePatch session objId = runAuthorization (throwError NotAuthorized) $ do
    sufficient $ sessionIsAdmin session
    sufficient $ sessionIsObject session objId
    sufficient $ sessionCreatedObject session objId



------------------------------------------------------------------------------
-- Authorization modules
------------------------------------------------------------------------------

-- | True if the session is an admin.
sessionIsAdmin :: Session -> AuthzModule
sessionIsAdmin session = elem (sessionObjId session) <$> adminObjIds


-- | True if the session created the given object.
sessionCreatedObject :: Session -> ObjId -> AuthzModule
sessionCreatedObject session objId = do
    obj <- Avers.lookupObject objId
    return $ objectCreatedBy obj == sessionObjId session

-- | True if the session is the given object.
sessionIsObject :: Session -> ObjId -> AuthzModule
sessionIsObject session objId = do
    return $ sessionObjId session == objId
