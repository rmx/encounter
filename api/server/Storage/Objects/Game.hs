{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Objects.Game
    ( gameObjectType

      -- * Views
    , gamesView
    , openGamesView
    , finishedGamesView
    ) where


import Control.Applicative
import Control.Monad.State

import qualified Database.RethinkDB as R

import Avers
import Storage.Objects.Game.Types

import Prelude



gameObjectType :: ObjectType Game
gameObjectType = ObjectType
    { otType   = "game"
    , otId     = ObjId <$> liftIO (newId 30)
    , otViews  = views
    }



------------------------------------------------------------------------------
-- Views

views :: [SomeView Game]
views =
    [ SomeView gamesView
    , SomeView openGamesView
    , SomeView finishedGamesView
    ]


-- | All games.
gamesView :: View Game Game
gamesView = View
    { viewName              = "games"
    , viewParser            = parseDatum
    , viewObjectTransformer = return . Just
    , viewIndices           = []
    }


-- | The @openGames@ view contains games which are in the 'Blank' or 'Setup'
-- stage.
openGamesView :: View Game Game
openGamesView = View
    { viewName              = "openGames"
    , viewParser            = parseDatum
    , viewObjectTransformer = \game@Game{..} -> return $ do
        guard $ gameStage == Blank || gameStage == Setup
        guard $ gamePurpose == Grading
        pure game
    , viewIndices           = []
    }


finishedGamesView :: View Game FinishedGame
finishedGamesView = View
    { viewName              = "finishedGames"
    , viewParser            = parseDatum
    , viewObjectTransformer = \game@Game{..} -> case (gamePurpose, gameStage) of
        (Grading, Finished) -> do
            obj <- lookupObject $ gameEncounterId game
            return $ Just $ FinishedGame
                gameEncounter
                (objectCreatedAt obj)
                gameDuration
                (winningParty game)
        _ -> return Nothing
    , viewIndices =
        [ SomeIndex leaderboardIndex
        ]
    }

leaderboardIndex :: Index (R.Array Double)
leaderboardIndex = Index "leaderboard" $ \obj -> R.lift
    [ R.GetField "objectId" (R.GetField "encounter" obj :: R.Exp R.Object)
    , R.Sub [R.lift (0::Double), R.GetField "score" (R.GetField "winningParty" obj :: R.Exp R.Object)]
    , R.GetField "duration" obj
    , R.GetField "createdAt" obj
    , R.GetField "id" obj
    ]
