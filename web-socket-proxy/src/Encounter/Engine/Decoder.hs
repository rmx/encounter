module Encounter.Engine.Decoder (messageAction) where

import qualified Data.List as L
import           Control.Applicative

import           Encounter.Engine.Types
import           Encounter.Engine.State
import           Encounter.Protocol.Messages


playerControlledObject :: Id -> Game (Maybe Id)
playerControlledObject account = do
    player <- L.find (\x -> _playerId x == account) <$> allPlayers
    return $ fmap _controlledObjectId player

withPlayerControlledObject :: Id -> (Id -> Game ()) -> Game ()
withPlayerControlledObject id' f = do
    id'' <- playerControlledObject id'
    case id'' of
        Nothing -> return ()
        Just x  -> f x


messageAction :: Id -> ClientMessage -> Game ()

messageAction _ CMsgConnect = do
    sendMessage (Id "") (SMsgBoot [] [] [] [])
    sendMessage (Id "") (SMsgSpells [])

messageAction accountId (CMsgMove pos) = do
    withPlayerControlledObject accountId $ \x -> do
        moveTo x pos
        sendToAllExcept (SMsgMove x pos) accountId

messageAction accountId (CMsgCast spellId target) = do
    withPlayerControlledObject accountId $ \x -> do
        castSpell x spellId target

messageAction _ _ = do
    return ()
