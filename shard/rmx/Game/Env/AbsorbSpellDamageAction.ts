
import { Aura, Spell } from '../../Pure/Game';
import { State } from '../../Game/State';
import { modifyAuraStackCount, modifyAuraEnergy } from '../../Game/Aura';
import { defineAccessor } from '../../Game/Env';
import { WorldObject } from '../../Game/Entities/WorldObject';



export function mk
( state  : State
, entity : WorldObject
, spell  : Spell
, aura   : Aura
, amount : number
) {
    var env: any = {};

    env.module = {};

    defineAccessor(env, 'entity', function() {
        return entity;
    });

    defineAccessor(env, 'spell', function() {
        return spell;
    });

    defineAccessor(env, 'amount', function() {
        return amount;
    });

    env.modifyStackCount = function(diff: number) {
        modifyAuraStackCount(state, aura, diff);
    };

    env.modifyEnergy = function(diff: number): void {
        modifyAuraEnergy(state, aura, diff);
    };


    return env;
}
