
import * as Avers from '../../vendor/avers';

import { Aura, lookupEntity, FormulaModifier } from '../Pure/Game';
import { clamp } from '../Pure/Math';
import { State } from '../Game/State';
import { broadcastMessage, scheduleFuture,
    loadScript, runExpression, uniqueId } from '../Game';
import { auraInfo, auraTickInterval, auraDuration } from '../Game/AuraInfo';
import * as Storage from '../Storage';
import { WorldObject } from '../Game/Entities/WorldObject';
import * as Message from '../Pure/Message';
import { OldGlobal } from '../Game/Global';
import * as AuraEffect from '../Game/AuraEffect';



export function
modifyAuraStackCount(state: State, aura: Aura, diff: number): void {
    // If stackCount is zero it means the aura doesn't use stacks, and
    // this function is a nop.
    if (aura.stackCount > 0) {
        aura.stackCount = clamp(0, 99, aura.stackCount + diff);

        // If the stack count falls to zero, then the aura is removed.
        if (aura.stackCount === 0) {
            var holder = lookupEntity<WorldObject>(state, aura.holderId);
            if (holder) {
                removeAura(state, holder, aura.slot);
            }

        } else {
            broadcastMessage(state, new Message.SMSG_UPDATE_AURA(aura));
        }
    }
}


export function
modifyAuraEnergy(state: State, aura: Aura, diff: number): void {
    // Same logic as in 'modifyAuraStackCount'.
    if (aura.energy > 0) {
        // XXX: What should be the upper bound?
        aura.energy = clamp(0, +Infinity, aura.energy + diff);

        if (aura.energy === 0) {
            var holder = lookupEntity<WorldObject>(state, aura.holderId);
            if (holder) {
                removeAura(state, holder, aura.slot);
            }

        } else {
            broadcastMessage(state, new Message.SMSG_UPDATE_AURA(aura));
        }
    }
}


// FIXME: This returns only the first attribute modifier aura effect. The
// aura may have multiple defined.
export function
auraModifierFor(state: State, aura: Aura, name: string): FormulaModifier {
    var mod = auraInfo(state, aura.auraId).effects.filter(x => {
        return x.effect instanceof Storage.AttributeModifier;
    }).filter(x => {
        return x.effect.name === name;
    })[0];

    if (mod) {
        var env = new OldGlobal(state, {}, {});
        return runExpression<FormulaModifier>(mod.effect.content, env);
    }
}



// triggerAura
// -----------------------------------------------------------------------
//
// Execute the aura trigger.

function
triggerAura(state: State, aura: Aura): void {
    var holder = lookupEntity<WorldObject>(state, aura.holderId);
    if (holder && holder.auras[aura.slot] === aura) {
        var delay = aura.tickInterval;
        if (delay) {
            scheduleFuture(state, delay, 'triggerAura', state => {
                triggerAura(state, aura);
            });
        }

        dispatchAuraEvent(state, aura, 'aura-tick');
    }
}


function
dispatchAuraEvent(state: State, aura: Aura, event: string): void {
    var holder = lookupEntity<WorldObject>(state, aura.holderId);
    auraInfo(state, aura.auraId).eventHandlers.forEach(eventHandler => {
        if (holder && eventHandler.event === event) {
            var env = new OldGlobal(state, aura, holder);
            runExpression(eventHandler.content, env);
        }
    });
}


function
aurasViolatingUniquenessConstraint
( state  : State
, caster : WorldObject
, target : WorldObject
, ai     : Storage.Aura
): Aura[] {
    var constraints = ai.uniquenessConstraint;

    switch(constraints.condition) {
    // we can only have one instance of the aura on the target
    case 'target-category':
        return target.auraList.filter(a => {
            return auraInfo(state, a.auraId).category == ai.category;
        });

    // each caster can have one instance of the aura on the target
    case 'caster-target-category':
        return target.auraList.filter(a => {
            return auraInfo(state, a.auraId).category == ai.category &&
                   a.casterId == caster.id;
        });
    }

    return [];
}

function
combineAuras
( state         : State
, aurasOnTarget : Aura[]
, caster        : WorldObject
, auraInfo      : Storage.Aura
): { duration: number; stacks: number; } {
    var uniquenessConstraint = auraInfo.uniquenessConstraint
      , violationResolver    = uniquenessConstraint.violationResolver
      , fileName             = 'violationResolver/expression'
      , source               = 'module.exports = (existingAura, newAura) -> ' + violationResolver.source
      , env                  = new OldGlobal(state, {}, {})
      , script               = loadScript(fileName, source, env)
      , newAura              = { duration: auraDuration(state, caster, auraInfo), stacks: 0 };

    return <any> aurasOnTarget.reduce((newAura, existingAura) => {
        return script(existingAura, newAura);
    }, newAura);
}


// addAura
// -----------------------------------------------------------------------
//
// Add the given aura to a new slot on the target. This function returns
// the slot so you can remove the aura later (with `removeAura`).

function
addAura
( state  : State
, caster : WorldObject
, holder : WorldObject
, auraId : string
): string {
    var slot = <string> uniqueId(state)
      , ai   = auraInfo(state, auraId);

    var aura = new Aura
        ( caster.id
        , holder.id
        , slot
        , auraId
        , state.currentTime
        , ai.energy
        , ai.stackCount
        , auraDuration(state, caster, ai)
        , auraTickInterval(state, caster, ai)
        );

    holder.auras[slot] = aura;


    // Automatically remove the aura after the duration.
    // TODO: Handle case when duration is extended or shortened.
    if (aura.duration) {
        scheduleFuture(state, aura.duration, 'removeAura', state => {
            removeAura(state, holder, slot);
        });
    }


    // If the aura has a tick interval, schedule a first tick.
    if (aura.tickInterval) {
        scheduleFuture(state, aura.tickInterval, 'triggerAura', state => {
            triggerAura(state, aura);
        });
    }


    broadcastMessage(state, new Message.SMSG_UPDATE_AURA(aura));


    updateAuraEffects(state, holder, aura, ai);


    return slot;
}


// updateAuraEffects
// -----------------------------------------------------------------------

function
updateAuraEffects
( state  : State
, entity : WorldObject
, aura   : Aura
, ai     : Storage.Aura
): void {
    ai.effects.forEach(x => {
        updateAuraEffect(state, entity, aura, x);
    });
}

interface AuraEffectImpl {
    ( state      : State
    , entity     : WorldObject
    , aura       : Aura
    , auraEffect : Storage.AuraEffect<any>
    ): void;
}

function
nullAuraEffect
( state      : State
, entity     : WorldObject
, aura       : Aura
, auraEffect : Storage.AuraEffect<any>
): void {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(state, entity, aura, auraEffect);
}


function
auraEffectType(ae: Storage.AuraEffect<any>): string {
    return Avers.typeName(Storage.auraEffectTypes, ae.effect.constructor);
}

function
lookupAuraEffectImpl(ae: Storage.AuraEffect<any>): AuraEffectImpl {
    var type = auraEffectType(ae);

    switch (type) {
    case 'absorb-spell-damage'   : return nullAuraEffect;
    // ^ Implemented in 'applySpellDamage'.

    case 'attribute-modifier'    : return AuraEffect.attributeModifier;
    case 'possess'               : return nullAuraEffect;
    case 'spell-effect-immunity' : return nullAuraEffect;
    case 'stun'                  : return AuraEffect.stun;
    case 'substitute-behavior'   : return AuraEffect.substituteBehavior;
    case 'taunt'                 : return nullAuraEffect;
    case 'petrify'               : return nullAuraEffect;
    }

    throw new Error('Unknown AuraEffect: ' + type);
}

function
updateAuraEffect
( state      : State
, entity     : WorldObject
, aura       : Aura
, auraEffect : Storage.AuraEffect<any>
): void {
    var impl = lookupAuraEffectImpl(auraEffect);
    impl(state, entity, aura, auraEffect);
}


// applyAura
// -----------------------------------------------------------------------
//
// The implementation of adding an aura to a WorldObject. It handles all
// the details of validating the uniqueness constraints etc.

export function
applyAura
( state  : State
, caster : WorldObject
, target : WorldObject
, auraId : string
): string {
    var ai               = auraInfo(state, auraId)
      , aurasInViolation = aurasViolatingUniquenessConstraint(state, caster, target, ai);

    if (aurasInViolation.length > 0) {
        // FIXME: This is wrong. auras are indexed by the slot, not auraId.
        // existingAura will always be 'undefined'.
        var existingAura = target.auras[auraId];
        if (existingAura) {
            var aurasOnTarget = aurasInViolation.filter((x) => { return x.holderId == target.id; })
              , combinedAura  = combineAuras(state, aurasOnTarget, caster, ai)
              , diff          = combinedAura.stacks - existingAura.stackCount;

            modifyAuraStackCount(state, existingAura, diff);

            return null;

        } else {
            aurasInViolation.forEach(aura => {
                var holder = lookupEntity<WorldObject>(state, aura.holderId);
                if (holder) {
                    removeAura(state, holder, aura.slot);
                }
            });

            return addAura(state, caster, target, auraId);
        }

    } else {
        return addAura(state, caster, target, auraId);
    }
}


export function
removeAura
( state  : State
, entity : WorldObject
, slot   : string
): void {
    var aura = entity.auras[slot];
    if (!aura) {
        return;
    }

    delete entity.auras[slot];

    var msg = new Message.SMSG_REMOVE_AURA(entity.id, slot);
    broadcastMessage(state, msg);

    var ai = auraInfo(state, aura.auraId);
    updateAuraEffects(state, entity, aura, ai);
}


// modifyAuras
// -----------------------------------------------------------------------

export function
modifyAuras
( state  : State
, entity : WorldObject

, predicate : (aura: Aura) => boolean
  // ^ Only auras for which the predicate returns true will be modified.

, action : (state: State, entity: WorldObject, aura: Aura) => void
  // ^ The action which modifies the aura.

, maxApplications : number
  // ^ The maximum number of times this action will be applied.

, perAuraLimit : number
  // ^ The number of times the action will be applied to one particular
  // aura.

, maxAffectedAuras : number
  // ^ The number of auras which are affected.
): void {
    entity.auraList.filter(aura => {
        return predicate(aura);
    }).slice(0, Math.max(0, maxAffectedAuras)).forEach(aura => {
        var n = perAuraLimit;

        while (maxApplications-- > 0 && n-- > 0) {
            action(state, entity, aura);

            if (!entity.auras[aura.slot]) {
                break;
            }
        }
    });
}
