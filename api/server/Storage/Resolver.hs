{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Resolver
  ( collectEncounterAssets
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe

import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Aeson hiding (Object)
import           Data.Aeson.Types hiding (Object)

import           Avers
import           Avers.Types

import           Storage.Types
import           Storage.ObjectTypes
import           Storage.Resources

import           Prelude


-- Top-level object: type + snapshot.
-- Embedded resource: type + value.

-- Asset can be a Spell, Aura, Tile, Terrain, VisualKit, ParticleEffect, ...
-- Stored either as a top-level object or within an encounter.

-- Resources can point directly to assets or indirectly through other
-- resources.

type UnresolvedReference = (Text, Reference)


data ResolverState = ResolverState
    { bindings             :: Map Reference (Text, Value)
      -- ^ Resolved references.

    , unresolvedReferences :: [ UnresolvedReference ]
      -- ^ Stack of references which need to be resolved.

    , snapshotCache        :: Map ObjectId Snapshot
      -- ^ Cache of latest snapshots. TODO: Should be cached by Avers.

    } deriving (Show)


newtype Resolver a = Resolver
    { runResolver :: StateT ResolverState Avers a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadError AversError, MonadState ResolverState, MonadAvers)

fetchReferenceSnapshot :: Reference -> Resolver (Maybe Snapshot)
fetchReferenceSnapshot Reference{..} =
    case referenceObjectId of
        Nothing    -> return Nothing
        Just objId -> liftAvers $ Just <$> lookupLatestSnapshot (BaseObjectId objId)

resolvePathInSnapshot :: Path -> Snapshot -> Resolver (Maybe Value)
resolvePathInSnapshot path Snapshot{..} = return $ resolvePathIn path snapshotContent


lookupReference :: (FromJSON a) => Reference -> Resolver a
lookupReference ref@Reference{..} = do
    b <- gets bindings
    obj <- case M.lookup ref b of
        Just (_, a) -> return $ Just a
        Nothing -> do
            c <- gets snapshotCache
            case referenceObjectId of
                Nothing -> return Nothing
                Just objId -> do
                    case M.lookup (BaseObjectId objId) c of
                        Just v -> resolvePathInSnapshot (fromMaybe (Path "") referencePath) v
                        Nothing -> runMaybeT $ do
                            snapshot <- MaybeT $ fetchReferenceSnapshot ref
                            MaybeT $ do
                                cacheSnapshot (BaseObjectId objId) snapshot
                                return $ Just ()

                            MaybeT $ resolvePathInSnapshot (fromMaybe (Path "") referencePath) snapshot

    case obj of
        Nothing -> throwError $ AversError $ "Could not resolve reference: " <> (T.pack (show ref))
        Just x -> case parseMaybe parseJSON x of
            Nothing -> throwError $ AversError $ "Could not resolve reference: " <> (T.pack (show ref))
            Just y -> return y


cacheSnapshot :: ObjectId -> Snapshot -> Resolver ()
cacheSnapshot key snapshot = do
    modify $ \s@ResolverState{..} -> s
        { snapshotCache = M.insert key snapshot snapshotCache
        }



-- | Pop the next unresolved resource from the stack. Or return 'Nothing'
-- if all resources have been reselved.
nextUnresolvedReference :: Resolver (Maybe UnresolvedReference)
nextUnresolvedReference = do
    refs <- gets unresolvedReferences
    case refs of
        []     -> return Nothing
        (x:xs) -> do
            modify $ \s@ResolverState{..} -> s { unresolvedReferences = xs }
            return $ Just x


resolver :: Resolver ()
resolver = do
    mb <- nextUnresolvedReference
    case mb of
        Nothing -> return ()
        Just (objType, ref) -> do
            value <- resolveReference ref
            insertBinding ref objType value
            resolver


resolveReference :: Reference -> Resolver Value
resolveReference ref = do
    value <- lookupReference ref
    case parseMaybe parseJSON value of
        Nothing                     -> return value
        Just (Inline   _   content) -> return $ toJSON content
        Just (Modified _ _ content) -> return $ toJSON content
        Just (External _   ext)     -> resolveReference ext


insertBinding :: Reference -> Text -> Value -> Resolver ()
insertBinding key objType value = do
    refs <- expandResource objType value

    modify $ \s@ResolverState{..} -> s
        { bindings             = M.insert key (objType, value) bindings
        , unresolvedReferences = unresolvedReferences ++ refs
        }


expandResource :: Text -> Value -> Resolver [ UnresolvedReference ]
expandResource objType value =
    case lookupResourceExpander objType of
        Nothing -> throwError $ AversError $ "Unknown asset type: " <> objType
        Just expander ->
            case expander value of
                Left  e -> throwError $ AversError $ "Failed to validate: " <> objType <> ", reason: " <> (T.pack $ show e)
                Right a -> return $ nub a



collectEncounterAssets :: Snapshot -> Encounter -> Avers [Binding]
collectEncounterAssets Snapshot{..} encounter@Encounter{..} = do
    r <- execStateT (runResolver resolver) initialState
    return $ encounterBinding : (map (\(k,(t,v)) -> Binding k t (toJSON v)) $ M.toList $ bindings r)

  where
    initialState       = ResolverState M.empty (resourceReferences ++ classReferences) M.empty
    resourceReferences = map asUnresolvedReference encounterResources
    classReferences    = map (\Item{..} -> ("class", itemContent)) encounterClasses
    encounterRef       = Reference (Just $ objectIdBase snapshotObjectId) Nothing Nothing
    encounterBinding   = Binding encounterRef "encounter" (toJSON encounter)

    asUnresolvedReference :: Item Resource -> UnresolvedReference
    asUnresolvedReference Item{..} =
        ( resourceType itemContent
        , Reference (Just $ objectIdBase snapshotObjectId) (Just snapshotRevisionId) (Just $ Path $ "resources." <> itemId)
        )
