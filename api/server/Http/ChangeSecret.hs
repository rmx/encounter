{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.ChangeSecret
    ( handleChangeSecret
    ) where



import           Control.Monad

import           Data.Text (Text)

import           Snap (modifyResponse, setResponseCode, finishWith, getResponse)

import           Avers
import           Avers.TH

import           Types
import           Session
import           Http



data Request = Request
  { reqSecret :: Text
  }



handleChangeSecret :: RequestHandler ()
handleChangeSecret = do
    accId <- authenticatedAccountId
    Request{..} <- parseRequestBody

    void $ reqAvers $ updateSecret (SecretId accId) reqSecret

    modifyResponse $ setResponseCode 200
    finishWith =<< getResponse



$(deriveJSON (deriveJSONOptions "req") ''Request)
