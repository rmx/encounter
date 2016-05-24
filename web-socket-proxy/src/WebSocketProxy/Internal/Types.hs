{-# LANGUAGE OverloadedStrings #-}

module WebSocketProxy.Internal.Types where


import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)

import           Control.Applicative

import           Encounter.Protocol.Internal
import           Prelude


data CMsgAuth = CMsgAuth
    { authToken  :: Text
    , authTarget :: RoutingInfo
    }


instance FromJSON CMsgAuth where
    parseJSON (Object o) = o .: "opcode" >>= parseJSON >>= msg
      where

        msg :: Int -> Parser CMsgAuth
        msg 0x0001 = CMsgAuth <$> o .: "token" <*> o .: "bind"
        msg _      = fail "CMsgAuth"

    parseJSON _ = fail "CMsgAuth"



data SMsgProfile = SMsgProfile
    { profileId :: Text
    , profileNick :: Text
    , profileGravatar :: Text
    }


instance ToJSON SMsgProfile where
    toJSON x = object
        [ "opcode"   .= (0x1001 :: Int)
        , "id"       .= profileId x
        , "nick"     .= profileNick x
        , "gravatar" .= profileGravatar x
        ]


data SMsgDisconnect = SMsgDisconnect Text
instance ToJSON SMsgDisconnect where
    toJSON (SMsgDisconnect reason) = object
        [ "opcode"   .= (0xFFFF :: Int)
        , "reason"   .= reason
        ]


data CMsgConnect = CMsgConnect
instance ToJSON CMsgConnect where
    toJSON = const $ object [ "opcode" .= (0x2003 :: Int) ]
