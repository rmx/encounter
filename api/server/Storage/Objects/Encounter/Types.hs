{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Objects.Encounter.Types where


import           Data.Text (Text)

import           Avers.TH
import           Storage.Types



-----------------------------------------------------------------------------
-- | Encounter

data Encounter = Encounter
    { encounterName :: Text
    , encounterGlue :: Text
    , encounterResources :: [ Item Resource ]
    , encounterParties :: [ Item Party ]

    , encounterClasses :: [ Item Reference ]
      -- DEPRECATED: since we added support for multiple parties. Remove once
      -- all encounters have been migrated.

    , encounterTerrainInstances :: [ Item TerrainInstance ]

    , encounterScoringFunction :: ExtensibleExpression

    } deriving (Show)


normalizedEncounterParties :: Encounter -> [ Item Party ]
normalizedEncounterParties Encounter{..} =
    if length encounterParties == 0
        then [ Item "party0" (Party encounterClasses [] (TerrainPosition "" "")) ]
        else encounterParties



$(deriveEncoding (deriveJSONOptions "encounter") ''Encounter)
