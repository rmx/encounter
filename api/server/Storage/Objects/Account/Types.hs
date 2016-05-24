{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Storage.Objects.Account.Types where


import           Data.Text (Text)

import           Avers.TH



-----------------------------------------------------------------------------
-- | Account

data Account = Account
    { accountLogin :: Text
    , accountEmail :: Maybe Text
    } deriving (Show)



$(deriveEncoding (deriveJSONOptions "account") ''Account)
