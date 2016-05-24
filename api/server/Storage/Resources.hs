{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Storage.Resources
    ( lookupResourceExpander
    ) where

import           Data.Text (Text)
import           Data.Aeson

import           Avers

import           Storage.Types


type Expander = Value -> Either AversError [(Text,Reference)]

expandCreature :: Creature -> [(Text,Reference)]
expandCreature Creature{..} =
    let toSpellRef Item{..} = ( "spell", itemContent )
    in [ ("model", creatureModel) ] ++ map toSpellRef creatureSpells

creatureExpander :: Expander
creatureExpander value = fmap expandCreature (parseValue value)

auraExpander :: Expander
auraExpander value = fmap expand (parseValue value)
  where
    expand Aura{..} = []


modelExpander :: Expander
modelExpander value = fmap expand (parseValue value)
  where
    expand Model{..} = []

spellExpander :: Expander
spellExpander value = fmap expand (parseValue value)
  where
    expand Spell{..} = []


behaviorExpander :: Expander
behaviorExpander value = fmap expand (parseValue value)
  where
    expand Behavior{..} = []


tileExpander :: Expander
tileExpander value = fmap expand (parseValue value)
  where
    expand Tile{..} = []

classExpander :: Expander
classExpander value = fmap expand (parseValue value)
  where
    expand Class{..} = expandCreature classCreature

terrainExpander :: Expander
terrainExpander value = fmap expand (parseValue value)
  where
    expand Terrain{..} =
        map (\(Item _ x) -> ("tile", tileInstanceRequestTile x)) terrainTiles

soundExpander :: Expander
soundExpander value = fmap expand (parseValue value)
  where
    expand Sound{..} = []

objectiveExpander :: Expander
objectiveExpander _ = return []

lookupResourceExpander :: Text -> Maybe Expander
lookupResourceExpander "aura"      = Just auraExpander
lookupResourceExpander "creature"  = Just creatureExpander
lookupResourceExpander "model"     = Just modelExpander
lookupResourceExpander "spell"     = Just spellExpander
lookupResourceExpander "behavior"  = Just behaviorExpander
lookupResourceExpander "tile"      = Just tileExpander
lookupResourceExpander "class"     = Just classExpander
lookupResourceExpander "terrain"   = Just terrainExpander
lookupResourceExpander "sound"     = Just soundExpander
lookupResourceExpander "objective" = Just objectiveExpander
lookupResourceExpander _           = Nothing
