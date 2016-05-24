{-# OPTIONS_GHC -fno-warn-orphans #-}

module Encounter.Protocol.Messages where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Vector as V

import           Control.Applicative

import           Encounter.Engine.Types


instance ToJSON Id where
    toJSON = toJSON . _id

instance FromJSON Id where
    parseJSON x = Id <$> parseJSON x


instance ToJSON Player where
    toJSON x = object
        [ "id"              .= _playerId x
        , "nick"            .= _displayName x
        , "gravatar"        .= _gravatarUrl x
        ]

instance ToJSON Team where
    toJSON x = object
        [ "score"           .= _score x
        , "members"         .= toJSON (_members x)
        ]


instance ToJSON Model where
    toJSON x = object
        [ "id"              .= _modelId x
        ]


instance ToJSON Tile where
    toJSON x = object
        [ "id"              .= _tileId x
        ]


instance ToJSON Sound where
    toJSON x = object
        [ "id"              .= _soundId x
        ]

instance ToJSON SpellTarget where
    toJSON LocationTarget = object
        [ "type" .= ("Location" :: String)
        ]

    toJSON WorldObjectTarget = object
        [ "type" .= ("WorldObject" :: String)
        ]

instance FromJSON SpellTarget where
    parseJSON (Object o) = o .: "type" >>= parseJSON >>= target
      where
        target :: String -> Parser SpellTarget
        target "Location"    = return LocationTarget
        target "WorldObject" = return WorldObjectTarget
        target _ = fail "SpellTarget"

    parseJSON _ = fail "SpellTarget"

instance (ToJSON a) => ToJSON (Vector3 a) where
    toJSON (Vector3 x y z) = Array $ V.fromList [ toJSON x, toJSON y, toJSON z ]

instance (FromJSON a) => FromJSON (Vector3 a) where
    parseJSON (Array a) = do
        if 3 /= V.length a
            then fail "Vector3"
            else Vector3
                <$> parseJSON ((V.!) a 0)
                <*> parseJSON ((V.!) a 1)
                <*> parseJSON ((V.!) a 2)

    parseJSON _ = fail "Vector3"


instance ToJSON Position where
    toJSON x = object
        [ "terrainId"       .= _terrainId x
        , "coordinates"     .= _coordinates x
        , "orientation"     .= _orientation x
        ]

instance FromJSON Position where
    parseJSON (Object o) = Position
        <$> o .: "terrainId"
        <*> o .: "coordinates"
        <*> o .: "orientation"

    parseJSON _ = fail "Position"


data ServerMessage

    -- | Data that is static and does not change during the course of the game
    --   is sent in the initial BOOT message.
    = SMsgBoot
        { smsgTeams :: [ Team ]
        , smsgModels :: [ Id ]
        , smsgTiles :: [ Id ]
        , smsgSounds :: [ Id ]
        }

    | SMsgCreateObject
        { smsgId :: Id
        , smsgPosition :: Position
        , smsgModelId :: Id
        }

    | SMsgDestroyObject
        { smsgId :: Id
        }

    | SMsgSpells [ Id ]

    | SMsgMove
        { smsgId :: Id
        , smsgPosition :: Position
        }

    | SMsgWardrobe { smsgEncounterId, smsgRevisionId :: String }
    | SMsgJoin { smsgProfile :: Player, smsgRoleId :: Maybe String, smsgReady :: Bool }
    | SMsgLeave  { smsgPlayerId :: String }
    | SMsgRole { smsgPlayerId :: String, smsgRoleId :: Maybe String }
    | SMsgReady { smsgPlayerId :: String, smsgReady :: Bool }
    | SMsgGame { smsgGameId :: String }
    | SMsgFlash { smsgMessage :: String }
    | SMsgChatMessage { smsgSender :: String, smsgText :: String }


instance ToJSON ServerMessage where

    toJSON (SMsgWardrobe encounterId revisionId) = object
        [ "opcode"          .= (0x2013 :: Int)
        , "encounterID"     .= encounterId
        , "revisionID"      .= revisionId
        ]

    toJSON (SMsgJoin player roleId ready) = object
        [ "opcode"          .= (0x1004 :: Int)
        , "profile"         .= player
        , "roleID"          .= roleId
        , "ready"           .= ready
        ]

    toJSON (SMsgLeave playerId) = object
        [ "opcode"          .= (0x1014 :: Int)
        , "id"              .= playerId
        ]

    toJSON (SMsgRole playerId roleId) = object
        [ "opcode"          .= (0x1006 :: Int)
        , "accountID"       .= playerId
        , "roleID"          .= roleId
        ]

    toJSON x@(SMsgBoot _ _ _ _) = object
        [ "opcode"          .= (0x1000 :: Int)
        , "teams"           .= smsgTeams x
        , "models"          .= smsgModels x
        , "tiles"           .= smsgTiles x
        , "sounds"          .= smsgSounds x
        ]

    toJSON x@(SMsgCreateObject _ _ _) = object
        [ "opcode"          .= (0x1000 :: Int)
        , "id"              .= smsgId x
        , "position"        .= smsgPosition x
        , "modelId"         .= smsgModelId x
        ]

    toJSON x@(SMsgDestroyObject _) = object
        [ "opcode"          .= (0x1000 :: Int)
        , "id"              .= smsgId x
        ]

    toJSON (SMsgSpells spells) = object
        [ "opcode"          .= (0x1000 :: Int)
        , "spells"          .= spells
        ]

    toJSON x@(SMsgMove _ _) = object
        [ "opcode"          .= (0x1000 :: Int)
        , "id"              .= smsgId x
        , "position"        .= smsgPosition x
        ]

    toJSON x@(SMsgGame _) = object
        [ "opcode"          .= (0x1021 :: Int)
        , "gameID"          .= smsgGameId x
        ]

    toJSON (SMsgChatMessage sender text) = object
        [ "opcode"          .= (0x1002 :: Int)
        , "sender"          .= sender
        , "text"            .= text
        ]

    toJSON (SMsgReady playerId ready) = object
        [ "opcode"          .= (0x1007 :: Int)
        , "accountID"       .= playerId
        , "ready"           .= ready
        ]

    toJSON (SMsgFlash text) = object
        [ "opcode"          .= (0x1020 :: Int)
        , "text"            .= text
        ]


data ClientMessage

    = CMsgConnect
    | CMsgMove { cmsgPosition :: Position }
    | CMsgCast { cmsgSpellId :: String, cmsgSpellTarget :: SpellTarget }
    | CMsgChatMessage { cmsgSender :: String, cmsgText :: String }
    | CMsgRole { cmsgRole :: String }
    | CMsgReady { cmsgReady :: Bool }
    | CMsgInvite { cmsgAccountId :: String }
    | CMsgKick { cmsgAccountId :: String }
    deriving (Show)


instance FromJSON ClientMessage where
    parseJSON (Object o) = op >>= parseJSON >>= msg
      where

        op :: Parser Value
        op = (o .: "o") <|> (o .: "op") <|> (o .: "opcode")

        msg :: Int -> Parser ClientMessage
        msg 0x2003 = return CMsgConnect
        msg 0x1002 = CMsgChatMessage <$> o .: "sender" <*> o .: "text"
        msg 0x1006 = CMsgRole <$> o .: "roleID"
        msg 0x1007 = CMsgReady <$> o .: "ready"
        msg 0x2004 = CMsgMove <$> o .: "position"
        msg 0x2005 = CMsgCast <$> o .: "spellId" <*> o .: "target"
        msg 0x2007 = CMsgInvite <$> o .: "accountID"
        msg 0x2008 = CMsgKick <$> o .: "accountID"
        msg _      = fail "ClientMessage"

    parseJSON _ = fail "ClientMessage"

parseClientMessage :: Value -> Maybe ClientMessage
parseClientMessage value = case fromJSON value of
    Error _   -> Nothing
    Success a -> Just a

encodeServerMessage :: ServerMessage -> Value
encodeServerMessage = toJSON
