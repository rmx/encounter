module Encounter.Engine.State where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import           Data.Ord
import           Control.Monad.State

import           System.Random

import qualified Encounter.MongoDB.Models as DB

import           Encounter.Engine.Types
import           Encounter.Protocol.Messages


-- A future is a game action that is executed at a certain time in the future.
-- The future may be canceled of the object owning it is destroyed before the
-- 'executeAt' time.
data Future = Future
    { _futureId :: Id
    , _executeAt :: Time
    , _action :: Game ()
    }

instance Eq Future where
    a == b = _futureId a == _futureId b

instance Ord Future where
    compare a b = if byExecuteAt == EQ then byFutureId else byExecuteAt
        where
            byExecuteAt = comparing _executeAt a b
            byFutureId  = comparing _futureId  a b

-- All the stuff that is stored in the database is loaded once during game
-- initialization, so we don't have to perform database queries during the
-- game.
data Assets = Assets
    { _assetsSpells :: [ DB.Spell ]
    }


data GameState = GameState
    { _assets :: Assets
    , _idCounter :: Int
    , _time :: Time
    , _teams :: [ Team ]
    , _terrains :: Map Id Terrain
    , _worldObjects :: Map Id WorldObject
    , _objectives :: Map Id Objective
    , _achievements :: [ Achievement ]
    , _scheduler :: [ Future ]
    , _outputBuffer :: [ (Id, ServerMessage) ]
    , _prng :: StdGen
    }

newtype Game a = Game {
      unGame :: StateT GameState IO a
    }

instance Monad Game where
    (Game a) >>= f = Game $ do
        res <- a
        unGame $ f res

    return = Game . return

instance MonadIO Game where
    liftIO = Game . liftIO

instance Functor Game where
    fmap = liftM


sget :: Game GameState
sget = Game get

smodify :: (GameState -> GameState) -> Game ()
smodify f = Game $ modify f


newId :: Game Id
newId = do
    counter <- liftM _idCounter sget
    smodify $ \s -> s { _idCounter = counter + 1 }
    return $ Id $ show counter



-- Teams and players
------------------------------------------------------------------------------

allPlayers :: Game [ Player ]
allPlayers = do
    teams <- liftM _teams sget
    return $ concatMap _members teams



-- WorldObject manipulation
------------------------------------------------------------------------------

getWorldObject :: Id -> Game WorldObject
getWorldObject x = do
    worldObjects <- liftM _worldObjects sget
    case M.lookup x worldObjects of
        Nothing -> error "Could not find WorldObject"
        Just worldObject -> return worldObject

modifyWorldObject :: (WorldObject -> WorldObject) -> Id -> Game ()
modifyWorldObject f id' =
    smodify $ \s -> s { _worldObjects = M.adjust f id' (_worldObjects s) }

relocate :: Position -> WorldObject -> WorldObject
relocate position worldObject = worldObject { _position = position }

moveTo :: Id -> Position -> Game ()
moveTo id' position = do
    modifyWorldObject (relocate position) id'
    emitEvent $ Event "moveTo"

startSpellcast :: SpellInfo -> SpellTarget -> WorldObject -> WorldObject
startSpellcast _ _ worldObject = worldObject


-- Randomness
------------------------------------------------------------------------------

-- Helper to access the random number generator state.
getRandom :: (StdGen -> (r, StdGen)) -> Game r
getRandom f = do
    prng <- liftM _prng sget
    let (r, prng') = f prng
    smodify $ \x -> x { _prng = prng' }
    return r

randomInt :: Game Int
randomInt = getRandom next

randomIntBetween :: Int -> Int -> Game Int
randomIntBetween lo hi = getRandom $ randomR (hi,lo)

randomFloat :: Game Float
randomFloat = getRandom $ randomR (0,1)



-- Events
------------------------------------------------------------------------------

emitEvent :: Event -> Game ()
emitEvent _ = return ()



-- Futures
------------------------------------------------------------------------------

insertFuture :: Future -> Game ()
insertFuture future = do
    smodify $ \s -> s { _scheduler = L.sort $ L.insert future $ _scheduler s }

scheduleFuture :: Time -> Game () -> Game ()
scheduleFuture executeAt action = do
    id' <- newId
    insertFuture $ Future id' executeAt action



-- Spells
------------------------------------------------------------------------------

getSpellInfo :: String -> Game SpellInfo
getSpellInfo _ = do
    return undefined

castSpell :: Id -> String -> SpellTarget -> Game ()
castSpell x spellId target = do
    -- TODO:
    --  - Check if the player has the spell in its spellbook.
    --  - Check if the preconditions are satisfied (target range, power etc).

    spellInfo <- getSpellInfo spellId
    modifyWorldObject (startSpellcast spellInfo target) x



-- Replies
------------------------------------------------------------------------------

sendMessage :: Id -> ServerMessage -> Game ()
sendMessage account msg =
    smodify $ \s -> s { _outputBuffer = (account, msg) : _outputBuffer s }

sendToAllExcept :: ServerMessage -> Id -> Game ()
sendToAllExcept msg account = do
    players <- allPlayers
    mapM_ maybeSendToAccount $ map _playerId players

  where

    maybeSendToAccount c = unless (c /= account) $ do
        sendMessage account msg


mkGameState :: Assets -> [ Team ] -> GameState
mkGameState assets teams =
    GameState
        assets
        0
        (fromInteger 0)
        teams
        M.empty
        M.empty
        M.empty
        []
        []
        []
        (mkStdGen 0)

runGame :: Game a -> GameState -> Time -> IO GameState
runGame (Game m) s time = do
    (_, s') <- runStateT m s { _time = time, _outputBuffer = [] }
    return s'
