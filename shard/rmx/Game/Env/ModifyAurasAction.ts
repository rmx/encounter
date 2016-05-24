
import { Aura } from '../../Pure/Game';
import { State } from '../../Game/State';
import { WorldObject } from '../../Game/Entities/WorldObject';
import { modifyAuraEnergy, modifyAuraStackCount, removeAura } from '../../Game/Aura';



export function mk
( state  : State
, entity : WorldObject
, aura   : Aura
): any {
    var env: any = {};

    env.module = {};


    env.modifyStackCount = function(diff: number) {
        modifyAuraStackCount(state, aura, diff);
    };

    env.modifyEnergy = function(diff: number) {
        modifyAuraEnergy(state, aura, diff);
    };

    env.removeAura = function() {
        removeAura(state, entity, aura.slot);
    };


    return env;
}
