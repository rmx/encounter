{-# LANGUAGE OverloadedStrings #-}

module Encounter.Protocol.Internal
    ( RoutingInfo(..)

    , ClientPacket(..)
    , decodeClientPacket, encodeClientPacket, clientPacketMessage

    , ServerPacket(..)
    , decodeServerPacket, encodeServerPacket, serverPacketMessage

    ) where


import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy
import Control.Applicative
import Prelude



-- | The 'RoutingInfo' holds information where a 'ClientPacket' should
--   be routed to or where a 'ServerPacket' is coming from.

data RoutingInfo = RoutingInfo
    { routingType :: Text
    , routingId :: Text
    } deriving (Eq, Show, Ord)


instance FromJSON RoutingInfo where
    parseJSON (Object o) = RoutingInfo <$> (o .: "type") <*> (o .: "id")
    parseJSON _          = fail "RoutingInfo"


instance ToJSON RoutingInfo where
    toJSON x = object [ "type" .= routingType x, "id" .= routingId x ]



-- | A 'ClientPacket' is sent from the WebSocketProxy to a Shard. The proxy
--   wraps messages it receives from clients with the clients account ID and
--   routing info to which shard the message should be routed.

data ClientPacket = ClientPacket
    { cpktAccountId :: Text
    , cpktRoutingInfo :: RoutingInfo
    , cpktMessage :: Value
    } deriving (Show)


instance FromJSON ClientPacket where
    parseJSON (Object o) = ClientPacket
        <$> (o .: "accountID")
        <*> (o .: "target")
        <*> (o .: "msg")

    parseJSON _ = fail "ClientPacket"

instance ToJSON ClientPacket where
    toJSON x = object
        [ "accountID" .= cpktAccountId x
        , "target"    .= cpktRoutingInfo x
        , "msg"       .= cpktMessage x
        ]



-- | Decode a 'ClientPacket' from a lazy 'ByteString'.
decodeClientPacket :: ByteString -> Maybe ClientPacket
decodeClientPacket = decode


-- | Encode a 'ClientPacket' into a lazy 'ByteString'.
encodeClientPacket :: ClientPacket -> ByteString
encodeClientPacket = encode


-- | Extract the message from a 'ClientPacket'.
clientPacketMessage :: FromJSON a => ClientPacket -> Maybe a
clientPacketMessage pkt = case fromJSON $ cpktMessage pkt of
    Error _   -> Nothing
    Success a -> Just a


-- | A 'ServerPacket' is sent from a Shard through a WebSocketProxy to
--   clients. The Proxy unwraps the message from the packet and sends it to
--   the correct client.

data ServerPacket = ServerPacket
    { spktAccountId :: Text
    , spktRoutingInfo :: RoutingInfo
    , spktMessage :: Value
    } deriving (Show)


instance FromJSON ServerPacket where
    parseJSON (Object o) = ServerPacket
        <$> (o .: "accountID")
        <*> (o .: "target")
        <*> (o .: "msg")

    parseJSON _ = fail "ServerPacket"


instance ToJSON ServerPacket where
    toJSON x = object
        [ "accountID" .= spktAccountId x
        , "target"    .= spktRoutingInfo x
        , "msg"       .= spktMessage x
        ]



-- | Decode a 'ServerPacket' from a lazy 'ByteString'.
decodeServerPacket :: ByteString -> Maybe ServerPacket
decodeServerPacket = decode


-- | Encode a 'ClientPacket' into a lazy 'ByteString'.
encodeServerPacket :: ServerPacket -> ByteString
encodeServerPacket = encode


-- | Extract the message from a 'ServerPacket'.
serverPacketMessage :: FromJSON a => ServerPacket -> Maybe a
serverPacketMessage pkt = case fromJSON $ spktMessage pkt of
    Error _   -> Nothing
    Success a -> Just a
