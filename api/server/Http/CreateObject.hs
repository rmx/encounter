{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.CreateObject
    ( handleCreateObject
    ) where



import           Control.Monad.Except

import           Data.Aeson (Value)
import           Data.Text (Text)

import           Avers as Avers
import           Avers.TH

import           Types
import           Session
import           Http
import           Authorization



data Request = Request
  { reqType    :: Text
  , reqContent :: Value
  }

data Response = Response
  { _resId      :: ObjId
  , _resType    :: Text
  , _resContent :: Value
  }



handleCreateObject :: RequestHandler ()
handleCreateObject = do
    session <- requireSession

    Request{..} <- parseRequestBody
    objId <- reqAvers $ do
        authorizeObjectCreate session reqType

        (SomeObjectType ot) <- lookupObjectType reqType
        content <- case parseValueAs ot reqContent of
            Left e -> throwError e
            Right x -> return x

        Avers.createObject ot (sessionObjId session) content

    case objId of
        Left  e -> error (show e)
        Right x -> sendResponse $ Response x reqType reqContent



$(deriveJSON (deriveJSONOptions "req") ''Request)
$(deriveJSON (deriveJSONOptions "_res") ''Response)
