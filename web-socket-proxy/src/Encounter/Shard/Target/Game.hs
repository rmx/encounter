module Encounter.Shard.Target.Game (gameAction) where

import           Data.Maybe
import           Data.Time.Clock
import           Control.Monad
import           Control.Applicative
import           Control.Concurrent.STM

import           Encounter.MongoDB.Pipe
import           Encounter.MongoDB.Query
import qualified Encounter.MongoDB.Models as DB

import           Encounter.Protocol.Internal

import           Encounter.Engine.Types
import           Encounter.Engine.State
import           Encounter.Engine.Decoder
import           Encounter.Protocol.Messages
import           Encounter.Shard.Types


findGameById :: Pipe -> String -> IO (Maybe DB.Game)
findGameById p i = findById p "games" i


loadAssets :: Pipe -> DB.Game -> IO Assets
loadAssets _ _ = do
    return $ Assets []

prepareTeams :: DB.Game -> [ Team ]
prepareTeams game = map toTeam (DB.gameTeams game)
  where
    toTeam :: DB.GameTeam -> Team
    toTeam (DB.GameTeam _ score players) = Team score (map toPlayer players)

    toPlayer :: DB.GamePlayer -> Player
    toPlayer (DB.GamePlayer id' role) = Player (Id $ show id') "" "" (Id "") (Just $ Id $ show role)

flushOutputBuffer :: GameState -> ServerMessageQueue -> IO ()
flushOutputBuffer gameState queue = do
    forM_ (_outputBuffer gameState) $ \(id', msg) -> do
        atomically $ writeTQueue queue (_id id', encodeServerMessage msg)

gameAction :: Pipe -> RoutingInfo -> TargetAction
gameAction pipe routingInfo clientMessageQueue serverMessageQueue = do
    epoch <- getCurrentTime

    game <- fromJust <$> findGameById pipe (routingId routingInfo)
    assets <- loadAssets pipe game

    let teams = prepareTeams game
    let state = mkGameState assets teams

    go epoch state

  where

    go epoch s = do
        (account, time, message) <- atomically $ readTQueue clientMessageQueue
        let tdiff = fromRational $ toRational $ diffUTCTime time epoch
        let clientMessage = fromJust $ parseClientMessage message
        s' <- runGame (messageAction (Id account) clientMessage) s tdiff
        flushOutputBuffer s' serverMessageQueue
        go epoch s'
