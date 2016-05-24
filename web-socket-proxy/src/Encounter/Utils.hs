module Encounter.Utils where

import Control.Applicative
import Network.BSD hiding (hostName)
import Network.Socket

fullyQualifiedDomainName :: IO (Maybe String)
fullyQualifiedDomainName = do
    hostName <- Just <$> getHostName
    addrInfo <- head <$> getAddrInfo Nothing hostName Nothing
    fst <$> getNameInfo [] True False (addrAddress addrInfo)
