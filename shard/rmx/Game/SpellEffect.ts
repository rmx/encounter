
import * as Storage from '../Storage';
import { Spell, Aura, SpellTarget, lookupEntity, SpellTargetType } from '../Pure/Game';
import { Terrain, terrainPositionAt } from '../Pure/Terrain';
import { State } from '../Game/State';
import { runExpression, resolveTerrainPosition } from '../Game';
import { WorldObject, formulaModifiers, teleportTo, applyHeal, applySpellDamage, isImmune } from '../Game/Entities/WorldObject';
import { stdFormula } from '../Game';
import { applyAura, modifyAuras } from '../Game/Aura';
import { interruptSpellcast } from '../Game/Spell';
import { SpellDamageInfo } from '../Game/Types';
import { spellInfo } from '../Game/SpellInfo';
import { createGroundAreaEffect } from '../Game/Entities/GroundAreaEffect';



import * as ModifyAurasAction from './Env/ModifyAurasAction';
import * as ModifyAurasPredicate from './Env/ModifyAurasPredicate';





// SpellTarget resolution
// -------------------------------------------------------------------------
//
// These helper functions transform SpellTarget to the desired type. If that
// is not possible, an error is thrown.
//
// Ideally, the editor UI would warn if the user creates a combination of
// target resolver and effect which are not compatible.

function
asWorldObject
( state       : State
, spellTarget : SpellTarget
, fn          : (entity: WorldObject) => void
): void {
    if (spellTarget.type === SpellTargetType.WorldObject) {
        var entity = lookupEntity<WorldObject>(state, spellTarget.entityId);
        if (entity) {
            fn(entity);
        }

    } else {
        throw new Error("Unsupported SpellTarget type: " + SpellTargetType[spellTarget.type]);
    }
}

function
asTerrainPosition
( state       : State
, spellTarget : SpellTarget
, fn          : (pos /* TerrainPosition */) => void
): void {
    if (spellTarget.type === SpellTargetType.WorldObject) {
        var entity = lookupEntity<WorldObject>(state, spellTarget.entityId);
        if (entity) {
            fn(entity.terrainPosition);
        }

    } else if (spellTarget.type === SpellTargetType.Location) {
        // FIXME: The terrain entity Id should be included in the 'SpellTarget'
        var terrain : Terrain = null;
        state.entities.forEach(x => {
            if (x instanceof Terrain) {
                terrain = <Terrain> x;
            }
        });

        if (terrain) {
            var pos = terrainPositionAt(terrain, <any>spellTarget.location);
            if (pos) {
                fn(pos);
            }
        }

    } else {
        throw new Error("Unsupported SpellTarget type: " + SpellTargetType[spellTarget.type]);
    }
}



// SpellEffect implementations
// -------------------------------------------------------------------------

export function
heal
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.Heal>
) {
    asWorldObject(state, spellTarget, entity => {
        if (!isImmune(state, entity, spell, spellEffect)) {
            var amount = Math.floor(spellEffect.effect.amount);
            applyHeal(state, entity, caster, spell, amount);
        }
    });
}

export function
spellDamage
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.SpellDamage>
) {
    asWorldObject(state, spellTarget, entity => {
        if (!isImmune(state, entity, spell, spellEffect)) {

            // XXX: Why do we apply the formula to '1' and then multiply
            // the amount with the resulting factor instead of passing
            // the amount directly to the formula?

            var modifiers = formulaModifiers(state, caster, 'spellDamage')
              , factor    = stdFormula.apply(1.0, modifiers)
              , effect    = spellEffect.effect
              , amount    = Math.floor(factor * effect.amount)
              , sdi       = new SpellDamageInfo(spell, effect.school, amount);

            applySpellDamage(state, entity, caster, spell, sdi);
        }
    });
}

export function
leap
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.SpellDamage>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(spell, spellEffect);

    asTerrainPosition(state, spellTarget, pos => {
        teleportTo(state, caster, pos);
    });
}

export function
charge
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.Charge>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(spell, spellEffect);

    asTerrainPosition(state, spellTarget, pos => {
        teleportTo(state, caster, pos);
    });
}

export function
applyAuraEffect
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.ApplyAura>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(spell);

    asWorldObject(state, spellTarget, entity => {
        var auraId = spellEffect.effect.aura.toString();
        applyAura(state, caster, entity, auraId);
    });
}

export function
interruptSpellcastEffect
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.InterruptSpellcast>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(caster, spell, spellEffect);

    asWorldObject(state, spellTarget, entity => {
        interruptSpellcast(state, entity);
    });
}

export function
groundArea
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.GroundArea>
) {
    asTerrainPosition(state, spellTarget, terrainPosition => {
        createGroundAreaEffect
            ( state
            , caster
            , terrainPosition
            , spellEffect.effect.duration.base
            , spellInfo(state, spell.spellId).radius.base
            , spellEffect.effect.aura.toString()
            );
    });
}

export function
modifyAurasEffect
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.ModifyAuras>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(caster, spell);

    asWorldObject(state, spellTarget, entity => {

        // The predicate and action curerntly only support 'expression'
        // implementations.
        //
        // Note: The code parses the expression on each call, which
        // probably yields suboptimal performance.

        function predicate(aura: Aura): boolean {
            return runExpression<boolean>
                ( spellEffect.effect.predicate.content
                , ModifyAurasPredicate.mk(state, aura)
                );
        }

        function action(state: State, entity: WorldObject, aura: Aura): void {
            runExpression
                ( spellEffect.effect.action.content
                , ModifyAurasAction.mk(state, entity, aura)
                );
        }

        modifyAuras
            ( state
            , entity
            , predicate
            , action
            , spellEffect.effect.maxApplications
            , spellEffect.effect.perAuraLimit
            , spellEffect.effect.maxAffectedAuras
            );
    });
}

export function
teleport
( state       : State
, caster      : WorldObject
, spell       : Spell
, spellTarget : SpellTarget
, spellEffect : Storage.SpellEffect<Storage.Teleport>
) {
    // FIXME: unused variable warning.
    ((...args) => { return args; })(caster, spell);

    asWorldObject(state, spellTarget, entity => {
        var dest     = spellEffect.effect.destination
          , position = resolveTerrainPosition(state, dest.content);

        teleportTo(state, entity, position);
    });
}
