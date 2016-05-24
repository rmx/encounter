module Encounter.Shard.Target.Wardrobe (wardrobeAction) where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Bson as BSON
import           Control.Monad.State
import           Control.Applicative
import           Control.Concurrent.STM
import           Network.Gravatar
import qualified Data.Text as T


import           Encounter.MongoDB.Pipe
import           Encounter.MongoDB.Query
import           Encounter.MongoDB.Models
import           Encounter.MongoDB.Models.Wardrobe ()

import           Encounter.Protocol.Internal
import           Encounter.Protocol.Messages
import           Encounter.Engine.Types
import           Encounter.Shard.Types


data WardrobeState = WardrobeState
    { _wardrobe :: Wardrobe
    , _pipe :: Pipe
    , _serverMessageQueue :: ServerMessageQueue
    , _players :: Map Id Player
    }

newtype WardrobeM a = WardrobeM
    { unWardrobe :: StateT WardrobeState IO a
    }

instance Monad WardrobeM where
    (WardrobeM a) >>= f = WardrobeM $ do
        res <- a
        unWardrobe $ f res

    return = WardrobeM . return

instance MonadIO WardrobeM where
    liftIO = WardrobeM . liftIO

instance Functor WardrobeM where
    fmap = liftM

sget :: WardrobeM WardrobeState
sget = WardrobeM get

smodify :: (WardrobeState -> WardrobeState) -> WardrobeM ()
smodify f = WardrobeM $ modify f

findWardrobeById :: Pipe -> String -> IO (Maybe Wardrobe)
findWardrobeById p = findById p "wardrobes"

findAccountById :: String -> Pipe -> IO (Maybe Account)
findAccountById id' pipe = findById pipe "accounts" id'

playerFromAccount :: Account -> Player
playerFromAccount account = Player (Id $ show $ accountId account) (accountNick account) (gravatar defaultConfig (T.pack $ accountEmail account)) (Id "") Nothing

createGame :: WardrobeState -> IO Game
createGame wardrobeState = do
    let pipe = _pipe wardrobeState
    shard <- fromJust <$> findOpenShard pipe "game"
    let wardrobe = _wardrobe wardrobeState
    players <- mapM toGamePlayer $ M.toList $ _players wardrobeState
    teamId <- BSON.genObjectId
    let team = GameTeam teamId 0 players
    let game = Game (wardrobeId wardrobe) (shardId shard) "" (wardrobeRevisionId wardrobe) (wardrobeEncounterId wardrobe) players [team] "proper"
    saveGame pipe game
    return game

  where

    toGamePlayer (_, x) = do
        let roleId = fromJust $ _playerRoleId x
        return $ GamePlayer (read $ _id $ _playerId x) (read $ _id $ roleId)

sendMessage :: Id -> ServerMessage -> WardrobeM ()
sendMessage accountId' message = do
    queue <- liftM _serverMessageQueue sget
    liftIO $ atomically $ writeTQueue queue (_id accountId', encodeServerMessage message)

broadcast :: ServerMessage -> WardrobeM ()
broadcast msg = do
    players <- liftM _players sget
    forM_ (M.toList players) $ \(_, player) ->
        sendMessage (_playerId player) msg

runQuery :: (Pipe -> IO a) -> WardrobeM a
runQuery action = do
    pipe <- liftM _pipe sget
    liftIO $ action pipe

joinPlayer :: Player -> WardrobeM ()
joinPlayer player = do
    let id' = _playerId player
    smodify $ \s -> s { _players = M.insertWith (\_ new -> new) id' player (_players s) }

kickPlayerByAccountId :: String -> WardrobeM ()
kickPlayerByAccountId id' =
    smodify $ \s -> s { _players = M.delete (Id id') (_players s) }

modifyPlayer :: Id -> (Player -> Player) -> WardrobeM ()
modifyPlayer id' f =
    smodify $ \s -> s { _players = M.adjust f id' (_players s) }


sendSMsgWardrobe :: Id -> WardrobeM ()
sendSMsgWardrobe accountId' = do
    wardrobe <- liftM _wardrobe sget
    let msg = SMsgWardrobe (show $ wardrobeEncounterId wardrobe) (show $ wardrobeRevisionId wardrobe)
    sendMessage accountId' msg

messageAction :: Id -> ClientMessage -> WardrobeM ()
messageAction accountId' CMsgConnect = do
    sendSMsgWardrobe accountId'

    account <- runQuery $ findAccountById (_id accountId')
    joinPlayer $ playerFromAccount $ fromJust account

    players <- liftM _players sget
    forM_ (M.toList players) $ \(_, player) -> do
        let msgJoin = SMsgJoin player (fmap _id $ _playerRoleId player) False
        sendMessage accountId' msgJoin


messageAction accountId' (CMsgRole roleId) = do
    modifyPlayer accountId' $ \player ->
        player { _playerRoleId = Just $ Id roleId }

    broadcast $ SMsgRole (_id accountId') (Just roleId)

messageAction accountId' (CMsgReady _) = do
    -- TODO: Implement this properly.
    s <- sget
    game <- liftIO $ createGame s
    let msg = SMsgGame (show $ gameId game)
    sendMessage accountId' msg

messageAction accountId' (CMsgChatMessage sender text) = do
    let msg = SMsgChatMessage sender text
    sendMessage accountId' msg

messageAction _ (CMsgInvite invitedAccountId) = do
    let player = Player (Id invitedAccountId) "crobi" "" (Id "") Nothing
    joinPlayer player

    let msg = SMsgJoin player (fmap _id $ _playerRoleId player) False
    broadcast msg

messageAction _ (CMsgKick kickedAccountId) = do
    kickPlayerByAccountId kickedAccountId

    let msg = SMsgLeave kickedAccountId
    broadcast msg


messageAction _ msg = do
    liftIO $ putStrLn "Unknown message"
    liftIO $ print msg
    return ()

wardrobeAction :: Pipe -> RoutingInfo -> TargetAction
wardrobeAction pipe targetSpec clientMessageQueue serverMessageQueue = do
    wardrobe <- fromJust <$> findWardrobeById pipe (routingId targetSpec)
    go $ WardrobeState wardrobe pipe serverMessageQueue M.empty

  where

    go s = do
        (account, _, message) <- atomically $ readTQueue clientMessageQueue
        let clientMessage = fromJust $ parseClientMessage message
        let (WardrobeM m) = messageAction (Id account) clientMessage
        (_, s') <- runStateT m s
        go s'
