
import * as Avers from '../../vendor/avers';

import { _ } from 'underscore';

import * as Storage from '../Storage';
import { State } from '../Game/State';
import { EntityId } from '../Pure/Ids';
import { broadcastMessage, scheduleFuture, directMessage, uniqueId } from '../Game';
import { WorldObject, isAlive, combatDistance, spellCooldownActive, triggerSpellCooldown,
    modifyPower } from '../Game/Entities/WorldObject';
import { Spell, SpellTarget, lookupEntity, TargetInfo } from '../Pure/Game';
import * as SpellEffect from '../Game/SpellEffect';
import { spellInfo, spellCastTime, spellCooldown, spellPulseTime, spellPowerCost } from '../Game/SpellInfo';
import { createProjectile } from '../Game/Entities/Projectile';
import { lookupTargetResolverImpl } from '../Game/TargetResolvers';
import * as Message from '../Pure/Message';



function
applyTargetResolver
( state     : State
, caster    : WorldObject
, spellInfo : Storage.Spell
) {
    return function(a, fn: Storage.TargetResolverFunction) {
        var impl = lookupTargetResolverImpl(fn);

        if (impl) {
            return impl(state, caster, spellInfo, a);
        } else {
            throw new Error('Unknown TargetResolver');
        }
    };
}

enum SpellCastResult
    { Success
    , NotReady
    , AlreadyCasting
    , CasterDead
    , NotEnoughPower
    , TargetOutOfRange
    , UnknownFailure
    };


// FIXME: This should take the range modifiers on the caster into account.
function
targetInRange
( state       : State
, caster      : WorldObject
, spellInfo   : Storage.Spell
, spellTarget : SpellTarget
): boolean {
    var max, min, _ref, _ref1, _ref2, _ref3;
    var targetType = spellInfo.targetType;
    var target = lookupEntity<WorldObject>(state, spellTarget.entityId);

    if (targetType === 'self') {
        return target && target.id === caster.id;

    } else if (targetType === 'world-object') {
        _ref = spellInfo.range; min = _ref.min; max = _ref.max;
        return target && (min <= (_ref1 = combatDistance(state, caster, target)) && _ref1 <= max);

    } else if (targetType === 'location') {
        _ref2 = spellInfo.range; min = _ref2.min; max = _ref2.max;
        return (min <= (_ref3 = combatDistance(state, caster, target || <any> spellTarget.location)) && _ref3 <= max);

    } else {
        return false;
    }
}

function
checkSpellcast
( state       : State
, caster      : WorldObject
, spellId     : string
, spellInfo   : Storage.Spell
, spellTarget : SpellTarget
): SpellCastResult {
    if (!isAlive(caster)) {
        return SpellCastResult.CasterDead;
    }

    if (caster.currentSpell) {
        return SpellCastResult.AlreadyCasting;
    }

    if (spellCooldownActive(state, caster, spellId)) {
        return SpellCastResult.NotReady;
    }

    var powerCost = spellInfo.powerCost;
    if (powerCost) {
        if (powerCost.powerType !== caster.activePower) {
            return SpellCastResult.NotEnoughPower;
        }
        if (powerCost.amount > caster.power) {
            return SpellCastResult.NotEnoughPower;
        }
    }

    if (!targetInRange(state, caster, spellInfo, spellTarget)) {
        return SpellCastResult.TargetOutOfRange;
    }

    return SpellCastResult.Success;
}


function
applyEffect(state: State, spell: Spell): void {

    var targetInfos = collectTargetInfos(state, spell);
    var si = spellInfo(state, spell.spellId);

    if (si.projectile.enabled) {
        targetInfos.forEach(x => {
            createProjectile(state, spell, x, si.projectile.speed);
        });
    } else {
        targetInfos.forEach(x => {
            applySpellEffects(state, spell, x);
        });
    }
}


// finishSpellcast
// -----------------------------------------------------------------------
//
// Called when the spell successfully finishes casting. Here we apply some
// spell effects on the targets.

function
finishSpellcast(state: State, spell: Spell): void {
    var caster = lookupEntity<WorldObject>(state, spell.casterId);


    // Modern implementation of spell effects, including support for
    // projectiles and other good stuff.

    var targetInfos = collectTargetInfos(state, spell);

    var msg = new Message.SMSG_SPELLCAST_FINISH(
        spell.casterId, targetInfos);

    broadcastMessage(state, msg);

    applyEffect(state, spell);

    // state.emitEvent(spell, caster, caster, 'finishSpellcast', caster);
    caster.currentSpell = null;
}



// collectTargetInfos
// -----------------------------------------------------------------------
//
// Generate a list of all targets to which the spell effects should be
// applied. Here we also decide which spell effects actually hit the
// target and thus should be applied to it (spells can implement multiple
// effects).

function
collectTargetInfos(state: State, spell: Spell): TargetInfo[] {
    var caster      = lookupEntity<WorldObject>(state, spell.casterId)
      , spellTarget = spell.spellTarget
      , si          = spellInfo(state, spell.spellId)
      , ret         = [];


    // The target info in the 'ret' list which corresponds to the given
    // entity. May return nothing if the entity doens't have a target info
    // yet.
    function targetInfoForEntity(entityId: EntityId): TargetInfo {
        return _.find(ret, targetInfo => {
            return targetInfo.spellTarget.entityId === entityId;
        });
    }


    // Mark the spell target as being affected by the spell effect at the
    // given index.
    function addTarget(spellTarget: SpellTarget, index: number): void {
        var targetInfo = targetInfoForEntity(spellTarget.entityId);
        if (targetInfo) {
            targetInfo.effectMask |= 1 << index;
        } else {
            ret.push(new TargetInfo(spellTarget, 1 << index));
        }
    }


    // Iterate over all effects and build up the target info list from the
    // resolved targets.
    si.effects.forEach((effect, index) => {
        var fn           = applyTargetResolver(state, caster, si)
          , spellTargets = effect.targetResolver.chain.reduce(fn, spellTarget);

        // The target resolver can return either a list or a single
        // target. If it returns a list of targets then the spell applies
        // to all of them.

        if (Array.isArray(spellTargets)) {
            spellTargets.forEach(spellTarget => {
                addTarget(spellTarget, index);
            });
        } else {
            addTarget(spellTargets, index);
        }
    });


    return ret;
}



// applySpellEffects
// -----------------------------------------------------------------------
//
// Apply all relevant spell effects to the target.

export function
applySpellEffects(state: State, spell: Spell, targetInfo: TargetInfo): void {
    var caster = lookupEntity<WorldObject>(state, spell.casterId);

    spellInfo(state, spell.spellId).effects.forEach((spellEffect, index) => {
        if (targetInfo.effectMask & (1 << index)) {
            applySpellEffect(state, caster, spell, targetInfo.spellTarget, spellEffect);
        }
    });
}



// attemptSpellcast
// -----------------------------------------------------------------------
//
// The main entry function to start a spellcast. This is used both by
// scripts and in response to a CMSG_SPELLCAST message.
//
// The spellcast may fail to start if certain conditions are not met. If
// it succeeeds, then the current spell of the caster is set.

export function
attemptSpellcast
( state       : State
, caster      : WorldObject
, spellTarget : SpellTarget
, spellId     : string
): void {
    validateSpellName(caster, spellId);
    var si = spellInfo(state, spellId);

    var scr = checkSpellcast(state, caster, spellId, si, spellTarget);
    if (scr !== SpellCastResult.Success) {
        // Send SMSG_SPELLCAST_FAILED only to the caster.
        var msg = new Message.SMSG_SPELLCAST_FAILED(SpellCastResult[scr]);
        directMessage(state, caster, msg);

    } else {
        var castTime = spellCastTime(state, caster, si);
        var spell = new Spell
            ( uniqueId(state)
            , caster.id
            , spellId
            , spellTarget
            , state.currentTime
            , castTime
            , spellPulseTime(state, caster, castTime, si)
            , null
            );

        // Need to set the current spell before executing any scripts.
        caster.currentSpell = spell;

        // Trigger any callbacks, start cooldowns and consume resources
        triggerCooldowns(state, caster, spell);
        consumeResources(state, caster, spell);

        // Schedule the first trigger
        if(spell.pulseTime) {
           triggerPulse(state, caster);
        }


        // Send SMSG_SPELLCAST_START (to all clients).
        broadcastMessage
            ( state
            , new Message.SMSG_SPELLCAST_START
                ( spell.id
                , spell.casterId
                , spell.spellId
                , spell.spellTarget
                , spell.castTime
                )
            );

        // Schedule the finishSpellcast future.
        scheduleFuture(state, spell.castTime, 'finishSpellcast', state => {
            if (caster.currentSpell === spell) {
                finishSpellcast(state, spell);
            }
        });
    }
}



// abortSpellcast
// -----------------------------------------------------------------------
//
// This is used when the player purposefully aborts the spell casting, for
// examply by pressing the ESC button.

export function
abortSpellcast(state: State, entity: WorldObject): void {
    var spell = entity.currentSpell;
    if (spell) {
        // state.emitEvent(entity, entity, entity, 'abortSpellcast', entity);

        entity.currentSpell = null;

        var msg = new Message.SMSG_SPELLCAST_CANCEL(entity.id);
        broadcastMessage(state, msg);
    }
}



// interruptSpellcast
// -----------------------------------------------------------------------
//
// This is used when somebody interrupts the spell casting of another
// entity. This action is available as a spell effect.
//
// TODO: Send a COMBATEVENT to the client, including the reason (or
// entity) which caused the interruption, so we can show this properly in
// the client combatlog.

export function
interruptSpellcast(state: State, entity: WorldObject): void {
    var spell = entity.currentSpell;
    if (spell) {
        // state.emitEvent(entity, entity, entity, 'interruptSpellcast', entity);

        entity.currentSpell = null;

        var msg = new Message.SMSG_SPELLCAST_CANCEL(entity.id);
        broadcastMessage(state, msg);
    }
}


function
triggerPulse(state: State, entity: WorldObject): void {
    var spell = entity.currentSpell;

    if (spell && spell.pulseTime != null) {

        scheduleFuture(state, spell.pulseTime, 'triggerPulse', state => {
            triggerPulse(state, entity);
        });

        applyEffect(state, spell);
    }
}


function
triggerCooldowns(state: State, caster: WorldObject, spell: Spell) {
    var cooldown = spellInfo(state, spell.spellId).cooldown;
    if (cooldown.enabled) {
        var amount = spellCooldown(state, caster, spellInfo(state, spell.spellId));
        triggerSpellCooldown(state, caster, spell.spellId, amount);
    }
}

function
consumeResources(state: State, caster: WorldObject, spell: Spell) {
    var powerCost = spellInfo(state, spell.spellId).powerCost
      , amount    = spellPowerCost(state, caster, spellInfo(state, spell.spellId));

    modifyPower(state, caster, powerCost.powerType, -amount);
}



interface EffectImpl {
    ( state       : State
    , caster      : WorldObject
    , spell       : Spell
    , spellTarget : SpellTarget
    , spellEffect : Storage.SpellEffect<any>
    ): void;
}

function
lookupEffectImpl(type: string): EffectImpl {
    switch (type) {
    case 'apply-aura'          : return SpellEffect.applyAuraEffect;
    case 'charge'              : return SpellEffect.charge;
    case 'ground-area'         : return SpellEffect.groundArea;
    case 'heal'                : return SpellEffect.heal;
    case 'interrupt-spellcast' : return SpellEffect.interruptSpellcastEffect;
    case 'leap'                : return SpellEffect.leap;
    case 'modify-auras'        : return SpellEffect.modifyAurasEffect;
    case 'spell-damage'        : return SpellEffect.spellDamage;
    case 'teleport'            : return SpellEffect.teleport;
    }
}

function
applySpellEffect
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<any>
): void {
    var type   = Avers.typeName(Storage.spellEffectTypes, spellEffect.effect.constructor)
      , impl   = lookupEffectImpl(type);

    if (impl) {
        impl(state, caster, spell, spellTarget, spellEffect);
    } else {
        throw new Error('Unknown SpellEffect: ' + type);
    }
}


function
validateSpellName(entity: WorldObject, spellId: string): void {
    if (entity.spells.indexOf(spellId) === -1) {
        throw new Error(
            [ entity
            , " doesn't have the spell "
            , spellId
            , " in its spellbook"
            ].join('')
        );
    }
}
