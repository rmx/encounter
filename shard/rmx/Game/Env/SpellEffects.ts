
import { State } from '../../Game/State';
import { WorldObject } from '../../Game/Entities/WorldObject';
import { TerrainPosition } from '../../Pure/Terrain';
import { checkParams } from '../../Game/Env';
import { SpellDamageInfo } from '../../Game/Types';
import { createGroundAreaEffect } from '../../Game/Entities/GroundAreaEffect';
import { applyAura } from '../../Game/Aura';
import { interruptSpellcast } from '../../Game/Spell';
import { applyHeal, applySpellDamage } from '../../Game/Entities/WorldObject';



export function
mk
( state  : State
, caster : WorldObject
, origin : any
): any {
    var env: any = {};


    env.applyAura = function(target: WorldObject, auraId: string): void {
        checkParams('applyAura', arguments, WorldObject, String);
        applyAura(state, caster, target, auraId);
    };

    env.heal = function(target: WorldObject, amount: number): void {
        checkParams('heal', arguments, WorldObject, Number);
        applyHeal(state, caster, target, origin, Math.round(amount));
    };

    env.unitInterruptCast = function(entity: WorldObject): void {
        checkParams('unitInterruptCast', arguments, WorldObject);
        interruptSpellcast(state, entity);
    };

    env.unitCastInterrupt = env.unitInterruptCast;

    env.spellDamage = function(target: WorldObject, damageType: string, amount: number): void {
        checkParams('spellDamage', arguments, WorldObject, String, Number);
        var spellDamageInfo = new SpellDamageInfo(null, damageType, Math.floor(amount));

        // FIXME: We're passing null as the spell, not good!
        applySpellDamage(state, target, caster, origin, spellDamageInfo);
    };

    env.groundAreaEffect = function(pos: TerrainPosition, duration, radius, auraName): void {
        checkParams('groundAreaEffect', arguments, TerrainPosition, Number, Number, String);
        createGroundAreaEffect(state, caster, pos, duration, radius, auraName);
    };


    return env;
}
