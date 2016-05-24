{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

This module contains type definitions for objects stored in the database.

-}

module Storage.Types where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict
import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Applicative

import           Avers (ObjId(..), RevId, Path(..), BlobId(..))
import           Avers.TH

import qualified Database.RethinkDB as R


-----------------------------------------------------------------------------
-- | Optional

data Optional a = Optional
    { optionalEnabled :: Maybe Bool -- FIXME: Should be required.
    , optionalContent :: a
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Range

data Range a = Range
    { rangeMin :: a
    , rangeMax :: a
    } deriving (Show)



-----------------------------------------------------------------------------
-- | CastTime

data CastTime = CastTime
    { castTimeMin  :: Double
    , castTimeBase :: Double
    , castTimeMax  :: Double
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Radius

data Radius = Radius
    { radiusMin  :: Double
    , radiusBase :: Double
    , radiusMax  :: Double
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Creature

data Creature = Creature
    { creatureName     :: Text
    , creatureModel    :: Reference
    , creatureHealth   :: Range Int
    , creatureBehavior :: Reference
    , creatureSpells   :: [ Item Reference ]
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Reference

data Reference = Reference
    { referenceObjectId   :: Maybe ObjId
    , referenceRevisionId :: Maybe RevId
    , referencePath       :: Maybe Path
    } deriving (Eq, Ord)

instance Show Reference where
    show Reference{..} = (maybe "" (T.unpack . unObjId) referenceObjectId) ++ seqNr ++ path
        where
            seqNr = maybe "" (("@"++) . show) referenceRevisionId
            path = case referencePath of
                Nothing        -> ""
                Just (Path "") -> ""
                Just (Path x)  -> ":" ++ T.unpack x


-----------------------------------------------------------------------------
-- | ResourceContent

data ResourceContent
    = RCAura      Aura
    | RCBehavior  Behavior
    | RCClass     Class
    | RCCreature  Creature
    | RCObjective Objective
    | RCSpell     Spell
    | RCTerrain   Terrain
    deriving (Show)


instance ToJSON ResourceContent where
    toJSON (RCAura      x) = toJSON x
    toJSON (RCBehavior  x) = toJSON x
    toJSON (RCClass     x) = toJSON x
    toJSON (RCCreature  x) = toJSON x
    toJSON (RCObjective x) = toJSON x
    toJSON (RCSpell     x) = toJSON x
    toJSON (RCTerrain   x) = toJSON x

parseResourceContentJSON :: Text -> Value -> Parser ResourceContent
parseResourceContentJSON "aura"      v = RCAura      <$> parseJSON v
parseResourceContentJSON "behavior"  v = RCBehavior  <$> parseJSON v
parseResourceContentJSON "class"     v = RCClass     <$> parseJSON v
parseResourceContentJSON "creature"  v = RCCreature  <$> parseJSON v
parseResourceContentJSON "objective" v = RCObjective <$> parseJSON v
parseResourceContentJSON "spell"     v = RCSpell     <$> parseJSON v
parseResourceContentJSON "terrain"   v = RCTerrain   <$> parseJSON v
parseResourceContentJSON t       _ = fail $ "ResourceContent: " ++ show t


instance R.ToDatum ResourceContent where
    toDatum (RCAura      x) = R.toDatum x
    toDatum (RCBehavior  x) = R.toDatum x
    toDatum (RCClass     x) = R.toDatum x
    toDatum (RCCreature  x) = R.toDatum x
    toDatum (RCObjective x) = R.toDatum x
    toDatum (RCSpell     x) = R.toDatum x
    toDatum (RCTerrain   x) = R.toDatum x

parseResourceContentDatum :: Text -> R.Datum -> Parser ResourceContent
parseResourceContentDatum "aura"      v = RCAura      <$> R.parseDatum v
parseResourceContentDatum "behavior"  v = RCBehavior  <$> R.parseDatum v
parseResourceContentDatum "class"     v = RCClass     <$> R.parseDatum v
parseResourceContentDatum "creature"  v = RCCreature  <$> R.parseDatum v
parseResourceContentDatum "objective" v = RCObjective <$> R.parseDatum v
parseResourceContentDatum "spell"     v = RCSpell     <$> R.parseDatum v
parseResourceContentDatum "terrain"   v = RCTerrain   <$> R.parseDatum v
parseResourceContentDatum t       _ = fail $ "ResourceContent: " ++ show t




-----------------------------------------------------------------------------
-- | Resource

data Resource
    = Inline Text ResourceContent
    -- ^ An asset defined inline, can be easily edited.

    | External Text Reference
    -- ^ A reference to an unmodified external asset. Either a specific version
    -- or the latest one.

    | Modified Text Reference ResourceContent
    -- ^ An asset which was initially copied from a different location
    -- but subsequently changed.

    deriving (Show)



instance ToJSON Resource where
    toJSON (Inline t content) = object
        [ "type"      .= toJSON t
        , "content"   .= content
        ]

    toJSON (External t ref) = object
        [ "type"      .= toJSON t
        , "reference" .= toJSON ref
        ]

    toJSON (Modified t ref content) = object
        [ "type"      .= toJSON t
        , "reference" .= toJSON ref
        , "content"   .= content
        ]

instance FromJSON Resource where
    parseJSON (Object o) = modified <|> external <|> inline
      where
        content = do
            t <- o .: "type"
            c <- o .: "content"
            parseResourceContentJSON t c

        inline    = Inline   <$> o .: "type" <*> content
        external  = External <$> o .: "type" <*> o .: "reference"
        modified  = Modified <$> o .: "type" <*> o .: "reference" <*> content

    parseJSON _ = fail "Resource"

instance R.ToDatum Resource where
    toDatum (Inline t content) = R.object
        [ "type"      R..= R.toDatum t
        , "content"   R..= R.toDatum content
        ]

    toDatum (External t ref) = R.object
        [ "type"      R..= R.toDatum t
        , "reference" R..= R.toDatum ref
        ]

    toDatum (Modified t ref content) = R.object
        [ "type"      R..= R.toDatum t
        , "reference" R..= R.toDatum ref
        , "content"   R..= R.toDatum content
        ]

instance R.FromDatum Resource where
    parseDatum (R.Object o) = modified <|> external <|> inline
      where
        content = do
            t <- o R..: "type"
            c <- o R..: "content"
            parseResourceContentDatum t c

        inline    = Inline   <$> o R..: "type" <*> content
        external  = External <$> o R..: "type" <*> o R..: "reference"
        modified  = Modified <$> o R..: "type" <*> o R..: "reference" <*> content

    parseDatum _ = fail "Resource"


resourceType :: Resource -> Text
resourceType (Inline   t _)   = t
resourceType (External t _)   = t
resourceType (Modified t _ _) = t



-----------------------------------------------------------------------------
-- | Item
--
-- FIXME: Move this into Avers, make a 'Collection' type alias for a list of
-- items.

-- | Each list item has a unique 'id' field.
data Item a = Item { itemId :: Text, itemContent :: a }
    deriving (Show)

instance FromJSON a => FromJSON (Item a) where
    parseJSON x@(Object o) = Item <$> o .: "id" <*> parseJSON x
    parseJSON _ = fail "Item"

instance ToJSON a => ToJSON (Item a) where
    toJSON Item{..} = Object $ insert "id" (toJSON itemId) o
        where (Object o) = toJSON itemContent

instance R.FromDatum a => R.FromDatum (Item a) where
    parseDatum x@(R.Object o) = Item <$> o R..: "id" <*> R.parseDatum x
    parseDatum _ = fail "Item"

instance R.ToDatum a => R.ToDatum (Item a) where
    toDatum Item{..} = R.Object $ insert "id" (R.toDatum itemId) o
        where (R.Object o) = R.toDatum itemContent



-----------------------------------------------------------------------------
-- | Party

data Party = Party
    { partyClasses    :: [ Item Reference ]
    , partyObjectives :: [ Item Reference ]
    , partySpawnPoint :: TerrainPosition
    } deriving (Show)



-----------------------------------------------------------------------------
-- | TerrainPosition

data TerrainPosition = TerrainPosition
    { tpTerrainEntityId     :: Text
    , tpPointOfInterestName :: Text
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Class

data Class = Class
    { className     :: Text
    , classCreature :: Creature
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Vector3

data Vector3 a = Vector3 !a !a !a
    deriving (Show)



-----------------------------------------------------------------------------
-- | Sound

data Sound = Sound
  { soundBlobId :: Maybe BlobId
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Texture

data Texture = Texture
  { textureType   :: Text
  , textureBlobId :: Maybe BlobId
  } deriving (Show)


data TextureType = Diffuse | Specular | Normal
  deriving (Show)



-----------------------------------------------------------------------------
-- | File

data File = File
  { fileName   :: Text
  , fileBlobId :: Maybe BlobId
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Skin

data Skin = Skin
  { skinName        :: Text
  , skinTextures    :: [ Item Texture ]
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Model

data Model = Model
  { modelName   :: Text
  , modelFiles  :: [ Item File ]
  , modelSkins  :: [ Item Skin ]
  -- , modelAnimations   :: [ Item Animation ]
  -- , modelVertexLabels :: [ Item VertexLabel ]
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Rotation

data Rotation = Rot0 | Rot90 | Rot180 | Rot270
    deriving (Show)

instance ToJSON Rotation where
    toJSON Rot0   = toJSON (0 :: Int)
    toJSON Rot90  = toJSON (1 :: Int)
    toJSON Rot180 = toJSON (2 :: Int)
    toJSON Rot270 = toJSON (3 :: Int)

instance FromJSON Rotation where
    parseJSON (Number 0) = return Rot0
    parseJSON (Number 1) = return Rot90
    parseJSON (Number 2) = return Rot180
    parseJSON (Number 3) = return Rot270
    parseJSON _          = fail "Rotation"

instance R.ToDatum Rotation where
    toDatum Rot0   = R.toDatum (0 :: Int)
    toDatum Rot90  = R.toDatum (1 :: Int)
    toDatum Rot180 = R.toDatum (2 :: Int)
    toDatum Rot270 = R.toDatum (3 :: Int)

instance R.FromDatum Rotation where
    parseDatum (R.Number 0) = return Rot0
    parseDatum (R.Number 1) = return Rot90
    parseDatum (R.Number 2) = return Rot180
    parseDatum (R.Number 3) = return Rot270
    parseDatum _            = fail "Rotation"



-----------------------------------------------------------------------------
-- | TileInstanceRequest
--
-- TODO: Find a better name

data TileInstanceRequest = TileInstanceRequest
  { tileInstanceRequestTile     :: Reference
  , tileInstanceRequestPosition :: Vector3 Int
  , tileInstanceRequestRotation :: Rotation
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Terrain

data Terrain = Terrain
  { terrainName  :: Text
  , terrainTiles :: [ Item TileInstanceRequest ]
  , terrainPointsOfInterest :: [ Item PointOfInterest ]
  } deriving (Show)


data PointOfInterest = PointOfInterest
    { poiName     :: Text
    , poiPosition :: Vector3 Double
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Tile

data Tile = Tile
  { tileName     :: Text
  , tileModel    :: Model
  , tileSize     :: Vector3 Int
  , tileSurface  :: Surface
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Behavior

data Behavior = Behavior
  { behaviorName     :: Text
  , behaviorScript   :: Text
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Objective

data Objective = Objective
  { objectiveTasks :: [ Item Task ]
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Task

data Task
    = TKillCreature KillCreature
    | TKillParties  KillParties
    deriving (Show)

data KillCreature = KillCreature
    { kcCreature :: Reference
    , kcCount    :: Int
    } deriving (Show)

data KillParties = KillParties
    { kpDummy :: Maybe Int
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Surface

data Surface = Surface
  { surfaceVertices :: [ Float ]
  , surfaceFaces    :: [ Float ]
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Expression

data Expression = Expression
  { expressionLanguage :: Maybe Text
  , expressionSource   :: Text
  } deriving (Show)



-----------------------------------------------------------------------------
-- | ExtensibleExpression

data ExtensibleExpression = ExtensibleExpression
  { eeType    :: Text -- FIXME: Currently only "expression" supported.
  , eeContent :: Expression
  } deriving (Show)



-----------------------------------------------------------------------------
-- | AttributeModifierName

data AttributeModifierName
    = ModMaxHealth      -- "maxHealth"
    | ModMovementSpeed  -- "movementSpeed"



-----------------------------------------------------------------------------
-- | Modifier

data Modifier = Modifier
  { modifierName       :: Text -- AttributeModifierName
  , modifierExpression :: Expression
  , modifierConditions :: [Expression]
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Aura

data Aura = Aura
  { auraName                 :: Text
  , auraDescription          :: Maybe Text
  , auraEffects              :: [ Item AuraEffect ]
  , auraUniquenessConstraint :: AuraUniquenessConstraint
  , auraDuration             :: Optional Radius -- FIXME
  , auraTickTimer            :: TickTimer
  , auraEventHandlers        :: [ Item Value ] -- FIXME
  , auraEnergy               :: Int
  , auraStackCount           :: Int
  } deriving (Show)


data AUCCondition
    = AUCTargetCategory
    | AUCCasterTargetCategory
    | AUCLocalStacking
    deriving (Show)

data AuraUniquenessConstraint = AuraUniquenessConstraint
    { aucCondition :: AUCCondition
    , aucViolationResolver :: Expression
    } deriving (Show)

data TickTimer
    = TTCounter  Counter
    | TTInterval Interval
    deriving (Show)

data Counter = Counter
    { counterMin  :: Int
    , counterBase :: Int
    , counterMax  :: Int
    } deriving (Show)

data Interval = Interval
    { intervalMin  :: Int
    , intervalBase :: Int
    , intervalMax  :: Int
    } deriving (Show)



-----------------------------------------------------------------------------
-- | AuraEffect

data AuraEffect
    = AESpellEffectImmunity SpellEffectImmunity
    | AEAttributeModifier   AttributeModifier
    | AEStun                Stun
    | AEPossess             Possess
    | AETaunt               Taunt
    | AESubstituteBehavior  SubstituteBehavior
    | AEPetrify             Petrify
    | AEAbsorbSpellDamage   AbsorbSpellDamage
    deriving (Show)



-----------------------------------------------------------------------------
-- | SpellEffectImmunity

data SpellEffectImmunity
    = SEIExpression Expression
    | SEIOther
    deriving (Show)



-----------------------------------------------------------------------------
-- | AttributeModifier

data AttributeModifier = AttributeModifier
    { amName :: Text -- FIXME: Enumerate all attributes.
    , amType :: Text
    , amContent :: Expression
      -- ^ FIXME (type+content represent a variant property). Currently only
      -- 'Expression' supported.

    } deriving (Show)



-----------------------------------------------------------------------------
-- | Stun

data Stun = Stun
    { stunDummy :: Maybe Int
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Taunt

data Taunt = Taunt
    { tauntDummy :: Maybe Int
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Possess

data Possess = Possess
    { possessBehavior :: Reference
    } deriving (Show)



-----------------------------------------------------------------------------
-- | SubstituteBehavior

data SubstituteBehavior = SubstituteBehavior
    { substituteBehaviorBehavior :: Reference
    } deriving (Show)



-----------------------------------------------------------------------------
-- | Petrify

data Petrify = Petrify
    { petrifyDummy :: Maybe Int
    } deriving (Show)



-----------------------------------------------------------------------------
-- | AbsorbSpellDamage

data AbsorbSpellDamage = AbsorbSpellDamage
    { asdDummy :: Maybe Int
    } deriving (Show)



data SpellTargetType
    = STTSelf
    | STTWorldObject
    | STTLocation
    deriving (Show)


data SpellEffect = SpellEffect
    { seTargetResolver :: TargetResolver
    , seEffect :: SpellEffectEffect
    } deriving (Show)

instance FromJSON SpellEffect where
    parseJSON x@(Object o) = SpellEffect <$> o .: "targetResolver" <*> parseJSON x
    parseJSON _ = fail "SpellEffect"

instance ToJSON SpellEffect where
    toJSON SpellEffect{..} = Object $ insert "targetResolver" (toJSON seTargetResolver) o
        where (Object o) = toJSON seEffect

instance R.FromDatum SpellEffect where
    parseDatum x@(R.Object o) = SpellEffect <$> o R..: "targetResolver" <*> R.parseDatum x
    parseDatum _ = fail "SpellEffect"

instance R.ToDatum SpellEffect where
    toDatum SpellEffect{..} = R.Object $ insert "targetResolver" (R.toDatum seTargetResolver) o
        where (R.Object o) = R.toDatum seEffect


data TargetResolver = TargetResolver
    { trChain :: [ Item TargetResolverFunction ]
    } deriving (Show)

data TargetResolverFunction
    = TRFSelf Petrify -- FIXME
    | TRFParty Petrify -- FIXME
    | TRFPartyInArea Petrify -- FIXME
    | TRFEnemiesInArea Petrify -- FIXME
    deriving (Show)


data SpellEffectEffect
    = SEDummy Petrify -- FIXME
    | SEApplyAura ApplyAura
    | SECharge Petrify -- FIXME
    | SEGroundArea GroundArea
    | SEHeal Heal
    | SEInterruptSpellcast Petrify -- FIXME
    | SELeap Petrify -- FIXME
    | SEModifyAuras ModifyAuras
    | SESpellDamage SpellDamage
    | SETeleport Teleport
    deriving (Show)

data ApplyAura = ApplyAura
    { aaAura :: Reference
    } deriving (Show)

data GroundArea = GroundArea
    { gaAura :: Reference
    , gaDuration :: CastTime -- FIXME: Better type
    } deriving (Show)

data SpellDamage = SpellDamage
    { sdSchool :: Text
    , sdAmount :: Double
    } deriving (Show)

data Heal = Heal
    { healAmount :: Double
    } deriving (Show)


data ModifyAuras = ModifyAuras
    { maPredicate :: ExtensibleExpression
    , maAction :: ExtensibleExpression
    , maMaxApplications :: Int
    , maPerAuraLimit :: Int
    , maMaxAffectedAuras :: Int
    } deriving (Show)


data TeleportDestination
    = TDTerrainPosition TerrainPosition
    | TDDummy ()
    deriving (Show)

data Teleport = Teleport
    { teleportDestination :: TeleportDestination
    } deriving (Show)


-----------------------------------------------------------------------------
-- | Spell

data Spell = Spell
  { spellName           :: Text
  , spellDescription    :: Maybe Text
  , spellEffects        :: [ Item SpellEffect ]
  , spellTargetType     :: SpellTargetType
  , spellRange          :: Range Int
  , spellCastTime       :: CastTime
  , spellRadius         :: Radius
  , spellPowerCost      :: PowerCost
  , spellCooldown       :: Optional Cooldown
  , spellProjectile     :: Projectile
  , spellSensousEffects :: Value -- FIXME
  , spellPulseTimer     :: Value -- FIXME
  } deriving (Show)


data PowerCost = PowerCost
    { powerCostPowerType :: Text
    , powerCostAmount    :: Int
    } deriving (Show)

data Cooldown = Cooldown
    { cooldownMin  :: Double
    , cooldownBase :: Double
    , cooldownMax  :: Double
    } deriving (Show)

data Projectile = Projectile
    { projectileEnabled :: Bool
    , projectileSpeed   :: Double
    } deriving (Show)

-----------------------------------------------------------------------------
-- | Skybox

data Skybox = Skybox
  { skyboxName      :: Text
  } deriving (Show)



-----------------------------------------------------------------------------
-- | Icon

data Icon = Icon
  { iconName        :: Text
  } deriving (Show)




-----------------------------------------------------------------------------
-- | ParticleEffect

data ParticleEffect = ParticleEffect
  { peName :: Text
  } deriving (Show)



-----------------------------------------------------------------------------
-- | TerrainInstance

data TerrainInstance = TerrainInstance
    { tiTerrain :: Reference
    } deriving (Show)



-----------------------------------------------------------------------------
-- FIXME: The types below are not stored in the database, they do not belong
-- here

data EncounterContent = EncounterContent
    { ecEncounterId      :: ObjId
    , ecGlue             :: Text
    , ecResources        :: [ Binding ]
    , ecParties          :: [ Item Party ]
    , ecTerrainInstances :: [ Item TerrainInstance ]
    , ecScoringFunction  :: ExtensibleExpression
    }

data Binding = Binding
    { bindingReference :: Reference
    , bindingType      :: Text
    , bindingValue     :: Value
    } deriving (Eq, Show)



$(deriveEncoding (deriveJSONOptions "aa")                  ''ApplyAura)
$(deriveEncoding (deriveJSONOptions "am")                  ''AttributeModifier)
$(deriveEncoding (deriveJSONOptions "asd")                 ''AbsorbSpellDamage)
$(deriveEncoding (deriveJSONOptions "auc")                 ''AuraUniquenessConstraint)
$(deriveEncoding (deriveJSONOptions "aura")                ''Aura)
$(deriveEncoding (deriveJSONOptions "behavior")            ''Behavior)
$(deriveEncoding (deriveJSONOptions "binding")             ''Binding)
$(deriveEncoding (deriveJSONOptions "castTime")            ''CastTime)
$(deriveEncoding (deriveJSONOptions "class")               ''Class)
$(deriveEncoding (deriveJSONOptions "cooldown")            ''Cooldown)
$(deriveEncoding (deriveJSONOptions "counter")             ''Counter)
$(deriveEncoding (deriveJSONOptions "creature")            ''Creature)
$(deriveEncoding (deriveJSONOptions "ec")                  ''EncounterContent)
$(deriveEncoding (deriveJSONOptions "ee")                  ''ExtensibleExpression)
$(deriveEncoding (deriveJSONOptions "expression")          ''Expression)
$(deriveEncoding (deriveJSONOptions "file")                ''File)
$(deriveEncoding (deriveJSONOptions "ga")                  ''GroundArea)
$(deriveEncoding (deriveJSONOptions "heal")                ''Heal)
$(deriveEncoding (deriveJSONOptions "icon")                ''Icon)
$(deriveEncoding (deriveJSONOptions "interval")            ''Interval)
$(deriveEncoding (deriveJSONOptions "kc")                  ''KillCreature)
$(deriveEncoding (deriveJSONOptions "kp")                  ''KillParties)
$(deriveEncoding (deriveJSONOptions "ma")                  ''ModifyAuras)
$(deriveEncoding (deriveJSONOptions "model")               ''Model)
$(deriveEncoding (deriveJSONOptions "modifier")            ''Modifier)
$(deriveEncoding (deriveJSONOptions "objective")           ''Objective)
$(deriveEncoding (deriveJSONOptions "optional")            ''Optional)
$(deriveEncoding (deriveJSONOptions "party")               ''Party)
$(deriveEncoding (deriveJSONOptions "pe")                  ''ParticleEffect)
$(deriveEncoding (deriveJSONOptions "petrify")             ''Petrify)
$(deriveEncoding (deriveJSONOptions "poi")                 ''PointOfInterest)
$(deriveEncoding (deriveJSONOptions "possess")             ''Possess)
$(deriveEncoding (deriveJSONOptions "powerCost")           ''PowerCost)
$(deriveEncoding (deriveJSONOptions "projectile")          ''Projectile)
$(deriveEncoding (deriveJSONOptions "radius")              ''Radius)
$(deriveEncoding (deriveJSONOptions "range")               ''Range)
$(deriveEncoding (deriveJSONOptions "reference")           ''Reference)
$(deriveEncoding (deriveJSONOptions "sd")                  ''SpellDamage)
$(deriveEncoding (deriveJSONOptions "skin")                ''Skin)
$(deriveEncoding (deriveJSONOptions "skybox")              ''Skybox)
$(deriveEncoding (deriveJSONOptions "sound")               ''Sound)
$(deriveEncoding (deriveJSONOptions "tp")                  ''TerrainPosition)
$(deriveEncoding (deriveJSONOptions "spell")               ''Spell)
$(deriveEncoding (deriveJSONOptions "stun")                ''Stun)
$(deriveEncoding (deriveJSONOptions "substituteBehavior")  ''SubstituteBehavior)
$(deriveEncoding (deriveJSONOptions "surface")             ''Surface)
$(deriveEncoding (deriveJSONOptions "taunt")               ''Taunt)
$(deriveEncoding (deriveJSONOptions "teleport")            ''Teleport)
$(deriveEncoding (deriveJSONOptions "terrain")             ''Terrain)
$(deriveEncoding (deriveJSONOptions "texture")             ''Texture)
$(deriveEncoding (deriveJSONOptions "texture")             ''TextureType)
$(deriveEncoding (deriveJSONOptions "ti")                  ''TerrainInstance)
$(deriveEncoding (deriveJSONOptions "tile")                ''Tile)
$(deriveEncoding (deriveJSONOptions "tileInstanceRequest") ''TileInstanceRequest)
$(deriveEncoding (deriveJSONOptions "tr")                  ''TargetResolver)

$(deriveEncoding (variantOptions "type" "effect" "AE")     ''AuraEffect)
$(deriveEncoding (variantOptions "type" "effect" "SE")     ''SpellEffectEffect)
$(deriveEncoding (variantOptions "type" "content" "TRF")   ''TargetResolverFunction)
$(deriveEncoding (variantOptions "type" "content" "TD")    ''TeleportDestination)

$(deriveEncoding (defaultVariantOptions "SEI")             ''SpellEffectImmunity)
$(deriveEncoding (defaultVariantOptions "STT")             ''SpellTargetType)
$(deriveEncoding (defaultVariantOptions "T")               ''Task)
$(deriveEncoding (defaultVariantOptions "TT")              ''TickTimer)
$(deriveEncoding (defaultVariantOptions "AUC")             ''AUCCondition)

$(deriveEncoding defaultOptions                            ''Vector3)
