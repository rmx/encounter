
import * as Avers from '../../../vendor/avers';
import { Vec3, vec3 } from 'gl-matrix';

import { EntityId } from '../../Pure/Ids';
import { CooldownTimer, Power, Soul, SpellTarget, Aura, Spell, lookupEntity,
    IState, Reaction, controllingPlayer, destroyEntity, FormulaModifier,
    updateAttribute, Attribute, registerEntity, ThreatManager } from '../../Pure/Game';
import { clamp } from '../../Pure/Math';
import * as Event from '../../Pure/Game/Events';
import { Terrain, TerrainPosition, getWorldCoordinates, terrainPositionAt } from '../../Pure/Terrain';
import { State } from '../../Game/State';
import { broadcastMessage, emitEvent, stdFormula, scheduleFuture, directMessage,
    loadScript, lookupResource, runExpression, forEachEntity, MoveFlags, uniqueId,
    maxHealthFormula } from '../../Game';
import { auraInfo } from '../../Game/AuraInfo';
import { SpellTargetType } from '../../Pure/Game';
import { PointMovementGenerator } from '../../Game/movement-generators/point';
import { FollowMovementGenerator } from '../../Game/movement-generators/follow';
import { Elevation, ElevationTransition } from '../../Pure/Game/Elevation';
import { MovePath } from '../../Pure/MovePath';
import { interruptSpellcast } from '../../Game/Spell';
import { auraModifierFor } from '../../Game/Aura';
import { SMSG_MOVEPATH, SMSG_SPELL_COOLDOWNS, SMSG_FIELDS,
    SMSG_CREATE_WORLDOBJECT } from '../../Game/Messages';
import * as Message from '../../Pure/Message';
import { MovementGenerator, CombatSource, SpellDamageInfo } from '../../Game/Types';
import { enumToJSON } from '../../Pure/JSON';
import { Behavior, attachBehavior } from '../../Game/Behavior';
import * as Storage from '../../Storage';
import { toString } from '../../Pure/String';
import { CombatEvent, Heal, Damage, Kill, HealthMutation, MaxHealthMutation, EntityMutationEvent,
    RelocationEvent } from '../../Pure/Game/Events';


import * as SpellEffectImmunity from '../Env/SpellEffectImmunity';
import * as AbsorbSpellDamagePredicate from '../Env/AbsorbSpellDamagePredicate';
import * as AbsorbSpellDamageAmount from '../Env/AbsorbSpellDamageAmount';
import * as AbsorbSpellDamageAction from '../Env/AbsorbSpellDamageAction';


import { _ } from 'underscore';





var defaultMovementSpeed = 3.0;
var defaultPositionType = "ground";


export class WorldObject {

    creatureId        : string;
    // ^ If the WorldObject was created from a creature template, this
    // is the reference to the creature resource.

    name              : string;

    // Health
    soul              : Soul;
    health            : number;
    maxHealth         : Attribute;

    // Power
    powers            : { [id: string]: Power; };
    activePower       : string;

    // Position
    terrainPosition   : TerrainPosition;

    // Movement
    moveFlags         : MoveFlags;
    movePath          : MovePath;
    movementGenerator : MovementGenerator;


    spells            : string[];
    // ^ Really a Set<string> of spell Ids, but nodejs Set doesn't
    // support forEach and we need to be able to iterate over all spells.

    currentSpell      : Spell;
    // ^ The spell which the entity is currently casting. May be null.

    // Misc attributes
    scale             : Attribute;
    movementSpeed     : Attribute;

    auras             : { [id: string]: Aura; };
    phase             : string;
    faction           : string;
    modelId           : string;

    positionType      : string;

    targetId          : EntityId;

    _combatReach      : number;
    _boundingRadius   : number;

    // Behavior
    behaviorStack     : Behavior[];
    visibilityRange   : number;
    enemyReaction     : string;

    // Threat
    threatManager     : ThreatManager;

    // Cooldowns
    globalCooldown    : CooldownTimer;
    spellCooldowns    : { [spellId: string]: CooldownTimer; };

    elevation           : Elevation;
    elevationTransition : ElevationTransition;


    constructor
      ( public id: EntityId
      ) {
        this.creatureId         = null;
        this.name               = 'Entity#' + id;

        this.soul               = Soul.Present;
        this.health             = 1;
        this.maxHealth          = new Attribute(maxHealthFormula, this.health);

        this.powers             = Object.create(null);
        this.activePower        = null;

        this.terrainPosition    = null;
        this.moveFlags          = MoveFlags.None;

        this.movePath           = null;
        this.movementGenerator  = null;

        this.spells             = [];
        this.currentSpell       = null;

        this.scale              = new Attribute(stdFormula, 1.0);
        this.movementSpeed      = new Attribute(stdFormula, defaultMovementSpeed);

        this.auras              = Object.create(null);
        this.faction            = 'Unknown';

        this.positionType       = defaultPositionType;

        this.phase              = null;
        this.modelId            = null;

        this.targetId           = null;
        this._combatReach       = 0.5; //this.model.combatReach || 0.5;
        this._boundingRadius    = 0.5; //this.model.boundingRadius || 0.5;

        this.behaviorStack      = [];
        this.visibilityRange    = null;
        this.enemyReaction      = 'Aggressive';

        this.threatManager      = new ThreatManager([]);

        this.globalCooldown     = new CooldownTimer(0, 0);
        this.spellCooldowns     = Object.create(null);

        this.elevation           = null;
        this.elevationTransition = new ElevationTransition(0, Elevation.Ground);
    }

    toString() {
        return toString(this, 'WorldObject', 'id', 'name');
    }

    get terrainId() {
        return this.terrainPosition.terrainId;
    }

    get power() {
        return this.powers[this.activePower].value;
    }

    get auraList(): Aura[] {
        return _.values(this.auras);
    }

    get appearance() {
        return { modelId: this.modelId };
    }

    get combatReach() {
        return this._combatReach * this.scale.value;
    }
    get boundingRadius() {
        return this._boundingRadius * this.scale.value;
    }


/*
    _smooth(path) {
      var last_ok, last_smooth, pos, smooth_path, waypoint, _i, _len;
      smooth_path = [];
      last_smooth = path[0];
      last_ok = last_smooth;
      smooth_path.push(last_ok);
      for (_i = 0, _len = path.length; _i < _len; _i++) {
        waypoint = path[_i];
        pos = waypoint.getWorldCoordinates();
        if (!this._crudeIsWalkable(last_smooth.getWorldCoordinates(), waypoint.getWorldCoordinates())) {
          if (smooth_path[smooth_path.length - 1] !== last_ok) {
            smooth_path.push(last_ok);
          }
          last_smooth = waypoint;
        }
        last_ok = waypoint;
      }
      smooth_path.push(path[path.length - 1]);
      return smooth_path;
    }

    _crudeIsWalkable(from, to) {
      var current, delta, direction, distance, num_steps, step, _i;
      delta = 0.01;
      direction = vec3.create();
      vec3.subtract(direction, to, from);
      distance = vec3.length(direction);
      num_steps = distance / delta;
      vec3.normalize(direction, direction);
      vec3.scale(direction, direction, delta);
      current = vec3.clone(from);
      for (step = _i = 1; 1 <= num_steps ? _i <= num_steps : _i >= num_steps; step = 1 <= num_steps ? ++_i : --_i) {
        vec3.add(current, direction, current);
        if (!this.terrain.getTerrainPositionAt(current)) {
          return false;
        }
      }
      return true;
    }
*/
}


export interface WorldObjectTemplate {
    creatureId      ?: string;
    name             : string;
    health           : number;
    spells           : string[];
    model            : string;
    position         : TerrainPosition;

    faction         ?: string;

    maxPower        ?: number;
    visibilityRange ?: number;
    enemyReaction   ?: string;
    behavior        ?: string;
    scale           ?: number;
    movementSpeed   ?: number;
}

function updateField(obj, name: string, value) {
    if (value !== undefined) {
        obj[name] = value;
    }
}


// createPower
// -----------------------------------------------------------------------
//
// Create a new power type on the given entity. The power type must not
// exist on the entity.

function
createPower
( state    : State
, entity   : WorldObject
, type     : string
, value    : number
, maxValue : number
): void {
    // TODO: Check whether the power type does not already exist:
    // assert(entity.powers[type] === undefined)

    entity.powers[type] = new Power(value, maxValue);

    // TODO: Broadcast a message.

    // XXX: Suppress unused variable warning.
    (() => { return state; })();
}


// createWorldObject
// -----------------------------------------------------------------------
//
// Create a new WorldObject entity.
//
// TODO: This shouldn't return a WorldObject, but rather an EntityId.

export function
createWorldObject
( state    : State
, template : WorldObjectTemplate
): WorldObject {
    var id     = uniqueId(state)
      , entity = new WorldObject(id);

    entity.creatureId     = template.creatureId || null;
    entity.name           = template.name;

    entity.health         = template.health;
    entity.maxHealth.base = entity.health;
    refreshAttribute(entity.maxHealth, [], () => {});

    createPower(state, entity, 'energy', 100, 100);
    entity.activePower    = 'energy';

    entity.terrainPosition = template.position.clone();

    // Default elevation transition.
    entity.elevation = null;
    transitionElevation(state, entity, Elevation.Ground);

    entity.scale.base = template.scale || 1;
    refreshAttribute(entity.scale, [], () => {});

    entity.movementSpeed.base = template.movementSpeed || defaultMovementSpeed;
    refreshAttribute(entity.movementSpeed, [], () => {});

    template.spells.forEach(spell => {
        learnSpell(state, entity, spell.toString());
    });

    entity.modelId        = template.model.toString();

    updateField(entity,  'faction',          template.faction);
    updateField(entity,  'visibilityRange',  template.visibilityRange);
    updateField(entity,  'enemyReaction',    template.enemyReaction);

    registerEntity(state, entity);
    broadcastMessage(state, SMSG_CREATE_WORLDOBJECT(entity));


    // This needs to be *after* we send out SMSG_CREATE_WORLDOBJECT
    // because the behavior constructor may send out some messages which
    // reference this entity.

    if (template.behavior) {
        attachBehavior(state, entity, template.behavior, null);
    }

    return entity;
}



// applyHeal
// -----------------------------------------------------------------------

export function
applyHeal
( state  : State
, entity : WorldObject
, caster : WorldObject
, spell  : Spell
, amount : number
): void {
    if (isAlive(entity)) {
        var actualHeal = modifyHealth(state, entity, spell, amount)
          //, overheal   = amount - actualHeal;
          ;

        var event = new CombatEvent
            ( spell
            , entity.id
            , new Heal(amount)
            );

        emitEvent(state, event);
        broadcastMessage(state, new Message.SMSG_COMBAT_EVENT(event));

        var hostiles = [];
        forEachEntity<WorldObject>(state, WorldObject, worldObject => {
            if (hasThreatReference(worldObject, entity)) {
                hostiles.push(worldObject);
            }
        });

        var totalHealThreat  = actualHeal / 2
          , threatPerHostile = totalHealThreat / hostiles.length;

        hostiles.forEach(function(worldObject) {
            addThreat(state, worldObject, caster, threatPerHostile);
        });
    }
}



// absorbSpellDamage
// -----------------------------------------------------------------------

function
absorbSpellDamage
( state           : State
, entity          : WorldObject
, spell           : Spell
, spellDamageInfo : SpellDamageInfo
, aura            : Aura
, effect          : Storage.AbsorbSpellDamage
): void {
    var predicateEnv = AbsorbSpellDamagePredicate.mk
        (entity, spell, aura, null);

    if (runExpression<boolean>(effect.predicate.content, predicateEnv)) {
        var amountEnv = AbsorbSpellDamageAmount.mk
            (entity, spell, aura, null);

        var amount = runExpression<number>(effect.amount.content, amountEnv);
        if (amount > 0) {
            var actualAmount = spellDamageInfo.absorbDamage(amount);

            var actionEnv = AbsorbSpellDamageAction.mk
                (state, entity, spell, aura, actualAmount);
            runExpression<any>(effect.action.content, actionEnv);
        }
    }
}


// applySpellDamage
// -----------------------------------------------------------------------

export function
applySpellDamage
( state           : State
, entity          : WorldObject
, caster          : WorldObject
, spell           : Spell
, spellDamageInfo : SpellDamageInfo
): void {

    // Dead entities do not receive damage.
    if (!isAlive(entity)) {
        return;
    }

    // Handle damage-absorb aura effects.
    entity.auraList.forEach(x => {
        auraInfo(state, x.auraId).effects.forEach(auraEffect => {
            if (auraEffect.effect instanceof Storage.AbsorbSpellDamage) {
                var effect = <Storage.AbsorbSpellDamage> auraEffect.effect;
                absorbSpellDamage(state, entity, spell, spellDamageInfo, x, effect);
            }
        });
    });

    var remainingDamage = spellDamageInfo.remainingDamage
      , actualDamage    = -modifyHealth(state, entity, spell, -remainingDamage)
      , overkill        = remainingDamage - actualDamage;

    addThreat(state, entity, caster, actualDamage);


    var event = new CombatEvent
        ( spell
        , entity.id
        , new Damage
            ( spellDamageInfo.damageType
            , actualDamage
            , spellDamageInfo.absorbedDamage
            , overkill
            )
        );

    emitEvent(state, event);
    broadcastMessage(state, new Message.SMSG_COMBAT_EVENT(event));
}



// refreshAttribute
// -----------------------------------------------------------------------
//
// Refresh an attribute with the given modifiers. Calls the function if
// the attribute value has changed.

function
refreshAttribute
( attribute   : Attribute
, modifiers   : FormulaModifier[]
, whenChanged : (value: number) => void
): void {
    var value = attribute.value;
    updateAttribute(attribute, modifiers);

    if (value !== attribute.value) {
        whenChanged(attribute.value);
    }
}

function
refreshMovementSpeed
( state  : State
, entity : WorldObject
): void {
    refreshAttribute
    ( entity.movementSpeed
    , formulaModifiers(state, entity, 'movementSpeed')
    , movementSpeed => {
        var fields = { movementSpeed: movementSpeed };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));
    });
}

function
refreshScale
( state  : State
, entity : WorldObject
): void {
    refreshAttribute
    ( entity.scale
    , formulaModifiers(state, entity, 'scale')
    , scale => {
        var fields = { scale: scale };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));
    });
}

function
refreshMaxHealth
( state  : State
, entity : WorldObject
): void {
    refreshAttribute
    ( entity.maxHealth
    , formulaModifiers(state, entity, 'maxHealth')
    , maxHealth => {
        if (maxHealth < entity.health) {
            setHealth(state, entity, null, maxHealth);
        }

        var fields = { maxHealth: maxHealth };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));

        var mutation = new MaxHealthMutation
          , event    = new EntityMutationEvent(entity.id, mutation);

        emitEvent(state, event);
    });
}



// setHealth
// -----------------------------------------------------------------------

function
setHealth
( state  : State
, entity : WorldObject
, source : CombatSource
, health : number
): void {
    // FIXME: Do the comparison after clamping the health!
    if (health !== entity.health) {
        entity.health = clamp(0, entity.maxHealth.value, health);

        var fields = { health: entity.health };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));

        var mutation = new HealthMutation
          , event    = new EntityMutationEvent(entity.id, mutation);

        emitEvent(state, event);

        if (entity.health === 0 && entity.soul == Soul.Present) {
            fadeSoul(state, entity, source);
        }
    }
}


// modifyHealth
// -----------------------------------------------------------------------

function
modifyHealth
( state  : State
, entity : WorldObject
, source : CombatSource
, diff   : number
): number {
    var oldHealth = entity.health;
    setHealth(state, entity, source, oldHealth + diff);
    return entity.health - oldHealth;
}


// setSoul
// -----------------------------------------------------------------------

function
setSoul
( state  : State
, entity : WorldObject
, soul   : Soul
): void {
    if (entity.soul !== soul) {
        entity.soul = soul;

        var fields = { soul: enumToJSON(Soul, entity.soul) };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));

        // XXX: emitEvent(state, EventEnv.mkSoul(entity.id));
    }
}


// fadeSoul
// -----------------------------------------------------------------------
//
// Note: You MUST NOT call this when the soul is already fading.
//
// FIXME: In what order should things happen / events be emitted? Does it
// matter?
//
// TODO: Remove auras which do not persist across death.


function
fadeSoul
( state  : State
, entity : WorldObject
, source : CombatSource
): void {
    setSoul(state, entity, Soul.Fading);
    interruptSpellcast(state, entity);
    stopMoving(state, entity);

    // FIXME: Functions querying the threat manager should filter those
    // out.
    forEachEntity<WorldObject>(state, WorldObject, otherEntity => {
        removeFromThreatList(otherEntity, entity);
    });


    // Emit a CombatEvent.
    var event = new CombatEvent
        ( source
        , entity.id
        , new Kill()
        );

    emitEvent(state, event);
    broadcastMessage(state, new Message.SMSG_COMBAT_EVENT(event));


    // When the entity is /not/ player controlled, destroy it after
    // a short delay.
    if (!controllingPlayer(state, entity)) {
        scheduleFuture(state, 10, 'fadeSoul -> destroyEntity', state => {
            destroyEntity(state, entity.id);
        });
    }
}


// setMovePath
// -----------------------------------------------------------------------
//
// Set the movePath of the entity. The entity will automatically follow
// the path until it arrives at the final position.
//
// WARNING: The entity will immediately jump to the first waypoint. Make
// sure that the move path starts at the position where the entity is
// located at the current time.

export function
setMovePath
( state    : State
, entity   : WorldObject
, movePath : MovePath
): void {
    entity.movePath = movePath;
    var terrain = lookupEntity<Terrain>(state, entity.terrainId);

    // Immediately apply the first move. This makes the entity relocate
    // to the first waypoint.
    applyUpdate(state.currentTime);

    broadcastMessage(state, SMSG_MOVEPATH(entity));


    function mkAction(previousTime: number): () => void {
        return function() {
            if (entity.movePath === movePath) {
                applyUpdate(previousTime);
            }
        };
    }

    function resetMovePath() {
        entity.movePath = null;
        broadcastMessage(state, SMSG_MOVEPATH(entity));
    }

    function applyUpdate(previousTime: number): void {
        if (canMove(state, entity)) {
            var elapsed = state.currentTime - previousTime
              , speed   = entity.movementSpeed.value
              , vec     = movePath.moveAlong(elapsed, speed)
              , pos     = terrainPositionAt(terrain, vec);

            if (pos) {
                moveTo(state, entity, pos, MoveFlags.Forward);

                if (!movePath.done) {
                    scheduleFuture(state, 0.2, 'movePath', mkAction(state.currentTime));
                } else {
                    resetMovePath();
                }
            } else {
                console.log(
                    [ "Could not project "
                    , vec3.str(vec)
                    , " to the walkable mesh"
                    ].join('')
                );

                resetMovePath();
            }
        } else {
            scheduleFuture(state, 0.2, 'movePath', mkAction(state.currentTime));
        }
    }
}


// interruptMovementGenerator
// -----------------------------------------------------------------------

function
interruptMovementGenerator(entity: WorldObject): void {
    if (entity.movementGenerator) {
        entity.movementGenerator.interrupt();
        entity.movementGenerator = null;
    }
}


// mutateMovementGenerator
// -----------------------------------------------------------------------
//
// Push the movement generator to the top of the stack. The previous
// movement generator (if present) is interrupted and put on hold until
// the new MG finishes and is removed from the stack.
//
// This automatically invalidates any existing move path, so unless the
// new movement generator computes a fresh move path, the entity stops
// moving.
//
// TODO: Actually use a stack. Currently only one MG can be attached
// to an entity.

export function
mutateMovementGenerator
( state  : State
, entity : WorldObject
, mg     : MovementGenerator
): void {
    interruptMovementGenerator(entity);

    entity.movePath          = null;
    entity.movementGenerator = mg;

    // Just to make sure in case the new MG doesn't set a new movepath.
    broadcastMessage(state, SMSG_MOVEPATH(entity));

    mg.initialize();
}


// learnSpell
// -----------------------------------------------------------------------
//
// Add a new spell to the entities spellbook.

export function
learnSpell(state: State, entity: WorldObject, spellId: string): void {
    if (entity.spells.indexOf(spellId) === -1) {
        // This lookup here is to ensure that the spellId exists. The lookup
        // will throw an error if it doesn't.
        lookupResource<Storage.Spell>(state, spellId, 'spell');

        entity.spells.push(spellId);

        // FIXME: Send a Message to the client.
    }
}


// modifyPower
// -----------------------------------------------------------------------

export function
modifyPower
( state  : State
, entity : WorldObject
, type   : string
, diff   : number
): void {
    var oldValue = entity.powers[type].value
      , newValue = entity.powers[type].modify(diff);

    if (newValue != oldValue) {
        var msg = new Message.SMSG_ENTITY_POWER(entity.id, type, newValue);
        broadcastMessage(state, msg);
    }
}


// formulaModifiers
// -----------------------------------------------------------------------
//
// Returns a list of formula modifiers which are attached to the entity.
//
// TODO: This function needs 'State' because aura modifiers are written
// as small scripts and need to be parsed and executed (and for that the
// State is needed). We should parse the scripts only once, in the content
// provider.

export function
formulaModifiers
( state  : State
, entity : WorldObject
, name   : string
): FormulaModifier[] {
    return entity.auraList.map(x => {
        return auraModifierFor(state, x, name);
    }).filter(function(x) {
        return !!x;
    });
}


// formulaModifiersChanged
// -----------------------------------------------------------------------
//
// Recalculate all attributes which are affected by the given formula
// modifiers.

export function
formulaModifiersChanged
( state  : State
, entity : WorldObject
, names  : string[]
): void {
    names.forEach(name => {
        if (name === 'maxHealth') {
            refreshMaxHealth(state, entity);
        } else if (name === 'movementSpeed') {
            refreshMovementSpeed(state, entity);
        } else if (name === 'scale') {
            refreshScale(state, entity);
        }
    });
}


// setTargetId
// -----------------------------------------------------------------------

export function
setTargetId
( state  : State
, entity : WorldObject
, id     : EntityId
): void {
    if (entity.targetId !== id) {
        entity.targetId = id;

        var fields = { targetId: id };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));

        // XXX: emitEvent(state, EventEnv.mkTarget(entity.id));
    }
}


// say
// -----------------------------------------------------------------------
//
// Let the entity say some text out loud. This has no effect on the game
// mechanics. But saying something does emit an event, so scripts can
// react to it.

export function
say
( state   : State
, entity  : WorldObject
, message : string
): void {
    broadcastMessage(state, new Message.SMSG_SAY(entity.id, message));
    // XXX: emitEvent(state, EventEnv.mkSay(entity.id, message));
}


// triggerSpellCooldown
// -----------------------------------------------------------------------

export function
triggerSpellCooldown
( state    : State
, entity   : WorldObject
, spellId  : string
, duration : number
): void {
    entity.spellCooldowns[spellId] = new CooldownTimer(state.currentTime, duration);
    directMessage(state, entity, SMSG_SPELL_COOLDOWNS(entity));
}


// auraEffectsOfType
// -----------------------------------------------------------------------
//
// Return all aura effects of the given type which are attached to the
// entity.

export function
auraEffectsOfType<T>
( state  : State
, entity : WorldObject
, type   : string
): T[] {
    var ret = [];

    entity.auraList.forEach(x => {
        auraInfo(state, x.auraId).effects.forEach(auraEffect => {
            var effect   = auraEffect.effect
              , typeName = Avers.typeName(Storage.auraEffectTypes, effect.constructor);
            if (typeName === type) {
                ret.push(auraEffect.effect);
            }
        });
    });

    return ret;
}


// aurasOfType
// -----------------------------------------------------------------------
//
// Return all auras of the given type which are attached to the
// entity.

export function
aurasOfType
( state  : State
, entity : WorldObject
, type   : string
): Aura[] {
    var ret = [];

    entity.auraList.forEach(x => {
        auraInfo(state, x.auraId).effects.forEach(auraEffect => {
            var effect   = auraEffect.effect
              , typeName = Avers.typeName(Storage.auraEffectTypes, effect.constructor);
            if (typeName === type) {
                ret.push(x);
            }
        });
    });

    return _.uniq(ret);
}

// isImmune
// -----------------------------------------------------------------------
//
// Returns true if the entity is immune to effects from the given spell.

export function
isImmune
( state       : State
, entity      : WorldObject
, spell       : Spell
, spellEffect : Storage.SpellEffect<any>
): boolean {
    var immunityEffects = auraEffectsOfType<Storage.SpellEffectImmunity<any>>(state, entity, 'spell-effect-immunity');

    var spellEffectImmunity =  immunityEffects.some(x => {
        var env    = SpellEffectImmunity.mk(entity, spell, spellEffect)
          , source = 'module.exports = -> ' + x.content.source
          , script = loadScript('isImmune', source, env);

        return script();
    });

    if (spellEffectImmunity)
        return true;


    var petrifyEffects = auraEffectsOfType<Storage.Petrify>(state, entity, 'petrify');
    if (petrifyEffects.length > 0)
        return true;

    return false;
}


export function
triggerGlobalCooldown
( state    : State
, entity   : WorldObject
, duration : number
): void {
    var modifiers = formulaModifiers(state, entity, 'globalCooldown');
    duration = stdFormula.apply(duration, modifiers);

    entity.globalCooldown = new CooldownTimer(state.currentTime, duration);
    notifyCooldowns(state, entity);
}


export function
notifyCooldowns(state: State, entity: WorldObject): void {
    directMessage(state, entity, SMSG_SPELL_COOLDOWNS(entity));
}


export function
moveTo
( state     : State
, entity    : WorldObject
, pos       : TerrainPosition
, moveFlags : MoveFlags
): void {
    // checkTerrainTypeChanged(state, entity, entity.terrainPosition, pos);

    entity.moveFlags       = moveFlags;
    entity.terrainPosition = pos.clone();

    emitEvent(state, new RelocationEvent(entity.id));
}

export function
teleportTo
( state  : State
, entity : WorldObject
, pos    : TerrainPosition
): void {
    if (isAlive(entity)) {
        // checkTerrainTypeChanged(state, entity, entity.terrainPosition, pos);

        stopMoving(state, entity);
        entity.terrainPosition = pos.clone();

        // FIXME: Reset moveFlags
        // FIXME: Emit RelocationEvent

        broadcastMessage(state, new Message.SMSG_TELEPORT
            ( entity.id
            , entity.terrainPosition
            )
        );
    }
}


/*
// XXX: Verify if still needed, terrain types currently not implemented
// in the editor.
function
checkTerrainTypeChanged(state: State, entity: WorldObject, old_pos, new_pos) {
    var old_type = old_pos.getTerrainType()
      , new_type = new_pos.getTerrainType();

    if (old_type !== new_type) {
        if (new_type !== "") {
            applyAura(state, entity, entity, new_type);
        }

        removeAura(state, entity, old_type);
    }
}
*/


// aurasWithEffect
// -----------------------------------------------------------------------
//
// Return the list of all auras on the entity which have the given
// aura effect.
//
// FIXME: Pass in the aura effect constructor instead of a string, and use
// an instanceof check instead of `Avers.typeName(...)`.

function
aurasWithEffect
( state  : State
, entity : WorldObject
, type   : string
): Aura[] {
    var ret         = []
      , effectTypes = Storage.auraEffectTypes;

    entity.auraList.forEach(x => {
        auraInfo(state, x.auraId).effects.forEach(auraEffect => {
            var effect = auraEffect.effect;
            if (Avers.typeName(effectTypes, effect.constructor) === type) {
                ret.push(x);
            }
        });
    });

    return _.uniq(ret);
}


// topThreat
// -----------------------------------------------------------------------
//
// The entity with the highest threat to the given entity. The top threat
// may be temporarily overridden by the 'Taunt' aura effect.

export function
topThreat
( state  : State
, entity : WorldObject
): EntityId {
    var tauntAuras = aurasWithEffect(state, entity, 'taunt');
    if (tauntAuras.length > 0) {
        return tauntAuras[0].casterId;
    } else {
        return _.find(threatList(entity), entityId => {
            var aggressor = lookupEntity<WorldObject>(state, entityId);
            if (aggressor) {
                return isAlive(aggressor);
            } else {
                return false;
            }
        });
    }
}


// isAlive
// -----------------------------------------------------------------------

export function
isAlive(entity: WorldObject): boolean {
    return entity.soul == Soul.Present;
}


// combatDistance
// -----------------------------------------------------------------------
//
// The distance at which the given entity can start melee attacks on the
// target.
//
// FIXME: The second argument should be of type WorldObject *or* Vec3.

export function
combatDistance(state: IState, entity: WorldObject, target: WorldObject): number {
    var d;
    if (target instanceof WorldObject) {
        d = distanceToEntity(state, entity, target) - entity.combatReach - target.boundingRadius;
    } else {
        // `target` presumably a Vec3.
        d = distanceToLocation(state, entity, <any>target) - entity.combatReach;
    }
    return Math.max(d, 0);
}


// reactionTowards
// -----------------------------------------------------------------------
//
// The type of reaction the entity has against another entity.

export function
reactionTowards(entity: WorldObject, other: WorldObject): Reaction {
    if (entity.faction == other.faction) {
        return Reaction.Friendly;
    } else {
        return Reaction.Hostile;
    }
}


// distanceToEntity
// -----------------------------------------------------------------------

export function
distanceToEntity(state: IState, entity: WorldObject, other: WorldObject): number {
    if (entity.terrainId !== other.terrainId) {
        return +Infinity;
    } else {
        var terrain = lookupEntity<Terrain>(state, entity.terrainId)
          , pos1    = getWorldCoordinates(terrain, entity.terrainPosition)
          , pos2    = getWorldCoordinates(terrain, other.terrainPosition);

        return vec3.dist(pos1, pos2);
    }
}


// distanceToLocation
// -----------------------------------------------------------------------

export function
distanceToLocation(state: IState, entity: WorldObject, location: Vec3): number {
    var terrain = lookupEntity<Terrain>(state, entity.terrainId)
      , pos     = getWorldCoordinates(terrain, entity.terrainPosition);

    return vec3.dist(pos, location);
}


// distanceToSpellTarget
// -----------------------------------------------------------------------
//
// FIXME: This doens't take ento account if the spell target is in
// a different terrain than the caster.

export function
distanceToSpellTarget
( state       : IState
, entity      : WorldObject
, spellTarget : SpellTarget
): number {
    if (spellTarget.type === SpellTargetType.WorldObject) {
        var target = lookupEntity<WorldObject>(state, spellTarget.entityId);
        if (target) {
            return distanceToEntity(state, entity, target);
        } else {
            return +Infinity;
        }
    } else {
        return distanceToLocation(state, entity, <any>spellTarget.location);
    }
}


// ThreatManager
// -----------------------------------------------------------------------
//
// TODO: Investigate if it's possible to move this to rmx.Pure or
// a separate file which has threat related functions.

export function
threatList(entity: WorldObject): EntityId[] {
    return entity.threatManager.getThreatList();
}

export function
hasThreatReference(entity: WorldObject, other: WorldObject): boolean {
    return entity.threatManager.hasThreatReference(other.id);
}

export function
addThreat
( state     : State
, entity    : WorldObject // Entity to which the threat will be added.
, other     : WorldObject // Caster
, rawThreat : number
): void {
    if (canHaveThreatAgainst(entity, other)) {
        var modifiers = formulaModifiers(state, other, 'generatedThreat')
          , threat    = stdFormula.apply(rawThreat, modifiers);

        entity.threatManager.addThreat(other.id, threat);
    }
}

export function
setTopThreat(entity: WorldObject, other: WorldObject, factor: number): void {
    if (canHaveThreatAgainst(entity, other)) {
        entity.threatManager.setTopThreat(other.id, factor);
    }
}

export function
removeFromThreatList(entity: WorldObject, other: WorldObject): void {
    entity.threatManager.removeFromThreatList(other.id);
}

export function
canHaveThreatAgainst(entity: WorldObject, other: WorldObject): boolean {
    return Reaction.Hostile === reactionTowards(entity, other);
}

// END ThreatManager



// Cooldowns
// -----------------------------------------------------------------------

export function
globalCooldownActive(state: State, entity: WorldObject): boolean {
    return entity.globalCooldown.isActive(state.currentTime);
}

export function
spellCooldownActive(state: State, entity: WorldObject, spellId: string): boolean {
    var cooldownTimer = entity.spellCooldowns[spellId];
    if (cooldownTimer) {
        return cooldownTimer.isActive(state.currentTime);
    } else {
        return false;
    }
}

export function
spellCooldownTimer(entity: WorldObject, spellId: string): CooldownTimer {
    var ret = entity.globalCooldown;

    var timer = entity.spellCooldowns[spellId];
    if (timer) {
        if (timer.start + timer.duration > ret.start + ret.duration) {
            ret = timer;
        }
    }

    return ret;
}



// Movement
// -----------------------------------------------------------------------

export function
stopMoving(state: State, entity: WorldObject): void {
    entity.movePath = null;
    interruptMovementGenerator(entity);

    broadcastMessage(state, SMSG_MOVEPATH(entity));
}

export function
follow
( state    : State
, entity   : WorldObject
, target   : WorldObject
, distance : number
): void {
    if (isAlive(entity)) {
        var mg = new FollowMovementGenerator(state, entity, target, distance);
        mutateMovementGenerator(state, entity, mg);
    }
}

export function
walkTo
( state   : State
, entity  : WorldObject
, pos     : TerrainPosition
, backoff : number
): void {
    if (isAlive(entity)) {
        var mg = new PointMovementGenerator(state, entity, pos, backoff);
        mutateMovementGenerator(state, entity, mg);
    }
}

// TODO: Proper type for `type`.
export function
setPositionType(state: State, entity: WorldObject, type): void {
    entity.positionType = type;

    var fields = { positionType: type };
    broadcastMessage(state, SMSG_FIELDS(entity, fields));
}



export function
processEvent_WorldObject
( entity : WorldObject
, event  : Event
): void {
    var ref = entity.behaviorStack[0];
    if (ref) {
        ref.processEvent(event);
    }
}


export function
canMove(state: State, entity: WorldObject): boolean {
    var stunEffects = auraEffectsOfType<Storage.Stun>(state, entity, 'stun');
    if (stunEffects.length > 0)
        return false;

    var petrifyEffects = auraEffectsOfType<Storage.Petrify>(state, entity, 'petrify');
    if (petrifyEffects.length > 0)
        return false;

    return true;
}



// transitionElevation
// -----------------------------------------------------------------------
//
// Initiate a elevation transition.
//
// XXX: What if the target elevation is the same as the current elevation?
// XXX: What if we already are in a transition?

export function
transitionElevation
( state     : State
, entity    : WorldObject
, elevation : Elevation
): void {
    var elevationTransition = new ElevationTransition
        ( state.currentTime
        , elevation
        );

    entity.elevationTransition = elevationTransition;

    // The delay is currently fixed. But it should eventually be loaded
    // from the currently active model to match the animation duration.
    // Or we make the transitions always last a predefined duration.

    scheduleFuture(state, 2, 'transitionElevation', state => {
        if (entity.elevationTransition === elevationTransition) {
            entity.elevation = entity.elevationTransition.elevation;

            var elevation = enumToJSON(Elevation, entity.elevation);
            broadcastMessage(state, SMSG_FIELDS(entity, { elevation: elevation }));
        }
    });
}
