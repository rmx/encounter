import * as Storage from '../Storage';
import { State } from '../Game/State';
import { WorldObject, formulaModifiers } from '../Game/Entities/WorldObject';
import { clamp } from '../Pure/Math';
import { stdFormula, lookupResource } from '../Game';



export function
spellInfo(state: State, spellId: string): Storage.Spell {
    return lookupResource<Storage.Spell>(state, spellId, 'spell');
}



export function
spellCastTime
( state     : State
, caster    : WorldObject
, spellInfo : Storage.Spell
): number {
    var castTime  = spellInfo.castTime
      , modifiers = formulaModifiers(state, caster, 'castTime');

    return Math.max(castTime.min, stdFormula.apply(castTime.base, modifiers));
}


export function
spellPulseTime
( state     : State
, caster    : WorldObject
, castTime  : number
, spellInfo : Storage.Spell
): number {
    var modifiers  = formulaModifiers(state, caster, 'pulseTime');

    if (!spellInfo.pulseTimer.enabled) {
        return;
    }

    var pulseTimer = spellInfo.pulseTimer.content.content;

    if (pulseTimer instanceof Storage.Interval) {
        return stdFormula.apply(pulseTimer.base, modifiers);
    } else if(pulseTimer instanceof Storage.Counter) {
        var base = castTime / pulseTimer.base;
        return stdFormula.apply(base, modifiers);
    }
}


export function
spellPowerCost
( state     : State
, caster    : WorldObject
, spellInfo : Storage.Spell
): number {
    var powerCost = spellInfo.powerCost
      , modifiers = formulaModifiers(state, caster, 'powerCost');

    return Math.max(0, stdFormula.apply(powerCost.amount, modifiers));
}


export function
spellCooldown
( state     : State
, caster    : WorldObject
, spellInfo : Storage.Spell
): number {
    var cooldown  = spellInfo.cooldown.content
      , modifiers = formulaModifiers(state, caster, 'spellCooldown')
      , value     = stdFormula.apply(cooldown.base, modifiers);

    return clamp(cooldown.min, cooldown.max, value);
}
