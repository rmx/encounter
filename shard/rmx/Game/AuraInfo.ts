
import { State } from '../Game/State';
import { stdFormula, lookupResource } from '../Game';
import * as Storage from '../Storage';
import { WorldObject, formulaModifiers } from '../Game/Entities/WorldObject';



export function
auraInfo(state: State, auraId: string): Storage.Aura {
    return lookupResource<Storage.Aura>(state, auraId, 'aura');
}


// FIXME: Clamp between min/max.
export function
auraDuration
( state    : State
, caster   : WorldObject
, auraInfo : Storage.Aura
): number {
    var duration = auraInfo.duration;
    if (duration.enabled) {
        var modifiers = formulaModifiers(state, caster, 'duration');
        return stdFormula.apply(duration.content.base, modifiers);
    }
}


// FIXME: Once we have modifiers for the tick timer, we'll have to move
// this to rmx.Game.Aura in order to apply the modifiers to the timer.
// FIXME: The duration should scale with the modifiers on the caster.
// FIXME: Clamp between min/max.
export function
auraTickInterval
( state    : State
, caster   : WorldObject
, auraInfo : Storage.Aura
): number {
    var timer     = auraInfo.tickTimer.content
      , modifiers = formulaModifiers(state, caster, 'tickInterval');

    if (timer instanceof Storage.Interval) {
        return stdFormula.apply(timer.base, modifiers);

    } else if (timer instanceof Storage.Counter) {
        if (auraInfo.duration.enabled) {
            var base = auraInfo.duration.content.base / timer.base;
            return stdFormula.apply(base, modifiers);
        }
    }
}
