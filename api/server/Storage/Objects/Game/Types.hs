{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Storage.Objects.Game.Types where



import           Data.Text (Text)
import           Data.List
import           Data.Function
import           Data.Time

import           Avers
import           Avers.TH

import           Storage.Types (Reference(..))




-----------------------------------------------------------------------------
-- | Game

data Game = Game
    { gameEncounter :: !Reference
      -- ^ The encounter which the game is running. Only the 'objectId' and
      -- 'revisionId' fields are used, the path is always empty.

    , gamePurpose :: !Purpose
      -- The purpose for which this game was created.

    , gameShardId :: !ObjId
      -- ^ The shard which was assigned to this game.

    , gameStage :: !Stage
      -- ^ The current stage. Periodically updated by the shard.

    , gameDuration :: !Double
      -- ^ Time how long the game spent in the 'Running' stage. If the game is
      -- still in the 'Blank' or 'Setup' stage then this is zero.

    , gameParties :: ![Party]
      -- ^ The parties which are playing the game. This is only updated on
      -- stage transitions.
    } deriving (Show)


gameEncounterId :: Game -> ObjId
gameEncounterId Game{..} = case referenceObjectId gameEncounter of
    Nothing    -> error "gameEncounterId: no objectId"
    Just objId -> objId


-- | The party which has won the game. If no party managed to complete all
-- objectives then this function returns 'Nothing'. If more than one party
-- completed all objectives then the one with the highest score is picked.
winningParty :: Game -> Maybe Party
winningParty Game{..} = case sortBy compareScore partiesWithCompletedObjectives of
    []  -> Nothing
    x:_ -> Just x
  where
    compareScore :: Party -> Party -> Ordering
    compareScore = compare `on` ((-) 0 . partyScore)

    partiesWithCompletedObjectives = flip filter gameParties $ \Party{..} ->
        all objectiveIsCompleted partyObjectives



-----------------------------------------------------------------------------
-- | Stage

data Stage = Blank | Setup | Running | Finished | Abandoned | Crashed
    deriving (Show, Eq, Ord)



-----------------------------------------------------------------------------
-- | Purpose
--
-- The perpose for which a game is created.
--
--  - 'Verification': One of the designers has created the game to check
--    whether it behaves like expected. Such games will not be included in the
--    leaderboard and may be eventually garbage collected.
--
--  - 'Grading': Ordinary players create these types of games. The grade of
--    these games can be computed, so they are included in the leaderboard.

data Purpose = Verification | Grading
    deriving (Show, Eq, Ord)



-----------------------------------------------------------------------------
-- | Party

data Party = Party
    { partyId         :: !Text
    , partyScore      :: !Double
    , partyPlayers    :: ![Player]
    , partyObjectives :: ![Objective]
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Player

data Player = Player
    { playerId     :: !ObjId
    , playerRoleId :: !Text
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Objective

data Objective = Objective
    { objectiveId          :: !Text
    , objectiveIsCompleted :: !Bool
    } deriving (Show)



-----------------------------------------------------------------------------
-- | FinishedGame

data FinishedGame = FinishedGame
    { fgEncounter :: !Reference
    , fgCreatedAt :: !UTCTime
    , fgDuration :: !Double

    , fgWinningParty :: !(Maybe Party)
      -- ^ This is only present if the gaem was actually won by a party. If
      -- none of the parties managed to completed all their objectives, it
      -- means that the computer has won and this remains empty.

    } deriving (Show)



deriveRecordEncoding ''Game "game"
    [ ( "purpose", [|Grading|] )
    ]

$(deriveEncoding (deriveJSONOptions "party")     ''Party)
$(deriveEncoding (deriveJSONOptions "player")    ''Player)
$(deriveEncoding (deriveJSONOptions "objective") ''Objective)
$(deriveEncoding (deriveJSONOptions "fg")        ''FinishedGame)
$(deriveEncoding (deriveJSONOptions "")          ''Stage)
$(deriveEncoding (deriveJSONOptions "")          ''Purpose)
