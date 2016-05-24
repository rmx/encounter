/// <reference path="../data.ts" />

module rmx.Core {

    export function
    castTimeSummary(castTime: rmx.Storage.CastTime): string {
        var suffix = ' sec cast';

        if (castTime.base === 0) {
            return 'instant cast';
        } else if (castTime.base === castTime.min) {
            return '' + castTime.base + suffix;
        } else {
            var range = [castTime.min, ' - ', castTime.max].join("");
            return '' + range + suffix;
        }
    }

    export function
    spellSummary(spell: rmx.Storage.Spell): string {

        var powerCostValue = "";
        if(spell.powerCost.amount > 0)
            powerCostValue = [spell.powerCost.amount, ' ',
                              spell.powerCost.powerType, ', '].join("");

        var castTimeValue = castTimeSummary(spell.castTime) + ", ";
        var coolDown  = spell.cooldown.content;
        var coolDownValue = [coolDown.min, ' - ', coolDown.max].join("");
        if(coolDown.min == coolDown.max)
            coolDownValue = "" + coolDown.min;
        coolDownValue = coolDownValue + " sec cooldown. ";

        return [
              powerCostValue
            , castTimeValue
            , coolDownValue
            , spell.description
            , '.'
        ].join("");
    }
}
