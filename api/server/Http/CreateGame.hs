{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.CreateGame
    ( createGameHandler
    ) where



import           Control.Monad.State

import           Data.IORef
import qualified Data.OrdPSQ as PSQ
import           Data.Maybe

import           Avers as Avers
import           Avers.TH

import           Types
import           Session
import           Http
import           Storage.Types
import           Storage.ObjectTypes
import           Storage.Objects.Game.Types



data Request = Request
  { reqEncounterId :: !ObjId
  , reqPurpose     :: !(Maybe Purpose)
  , reqRevisionId  :: !(Maybe RevId)
  }

data Response = Response
  { _resGameId :: ObjId
  }



createGameHandler :: RequestHandler ()
createGameHandler = do
    session <- requireSession
    Request{..} <- parseRequestBody

    -- Find a suitable shard for the game.
    shardId <- do
        ref <- gets _shardRegistry
        reg <- liftIO $ readIORef ref
        case PSQ.findMin reg of
            Nothing -> failWith $ InternalServerError "createGameHandler: No shard available"
            Just (k,_,_) -> return k

    res <- reqAvers $
        Avers.createObject gameObjectType (sessionObjId session) $ Game
            { gameEncounter = Reference (Just reqEncounterId) reqRevisionId Nothing
            , gamePurpose   = fromMaybe Grading reqPurpose
            , gameShardId   = shardId
            , gameDuration  = 0
            , gameStage     = Blank
            , gameParties   = []
            }

    replyWith res (sendResponse . Response)



$(deriveJSON (deriveJSONOptions "req") ''Request)
$(deriveJSON (deriveJSONOptions "_res") ''Response)
