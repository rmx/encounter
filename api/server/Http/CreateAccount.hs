{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.CreateAccount
    ( createAccountHandler
    ) where



import           Data.Text (Text)

import           Avers
import           Avers.TH

import           Types
import           Http
import           Storage.ObjectTypes



data Request = Request
  { reqLogin :: Text
  }

data Response = Response
  { _resAccountId :: ObjId
  }



createAccountHandler :: RequestHandler ()
createAccountHandler = do
    Request{..} <- parseRequestBody
    let content = Account reqLogin Nothing

    res <- reqAvers $ do
        accId <- Avers.createObject accountObjectType rootObjId content
        updateSecret (SecretId (unObjId accId)) ""
        return accId

    replyWith res (sendResponse . Response)



$(deriveJSON (deriveJSONOptions "req")  ''Request)
$(deriveJSON (deriveJSONOptions "_res") ''Response)
