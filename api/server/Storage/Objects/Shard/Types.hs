{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Storage.Objects.Shard.Types where


import           Data.Text (Text)

import           Avers.TH



-----------------------------------------------------------------------------
-- | Shard

data Shard = Shard
    { shardVersion :: !Text
      -- ^ The engine version. The output of `git describe`.

    , shardAddress :: !(Text, Int)
      -- ^ The hostname and port where the shard is listening to incoming
      -- WebSocket connections.

    } deriving (Show)



$(deriveEncoding (deriveJSONOptions "shard") ''Shard)
