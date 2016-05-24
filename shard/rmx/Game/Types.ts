import { Spell, Aura } from '../Pure/Game';
import { Environment } from '../Pure/Game/Events';

// CombatSource
// -----------------------------------------------------------------------
//
// Something which can be the source of a combat action (eg. applyHeal).

export type CombatSource
    = Environment
    | Spell
    | Aura
    ;


// SpellDamageInfo
// -----------------------------------------------------------------------

export class SpellDamageInfo {

    absorbedDamage : number;

    constructor
      ( public spell           : Spell
      , public damageType      : string
      , public remainingDamage : number
      ) {
        this.absorbedDamage = 0;
    }

    absorbDamage(absorbAmount: number): number {
        var actualAbsorbAmount = Math.min(this.remainingDamage, Math.floor(absorbAmount));

        this.remainingDamage -= actualAbsorbAmount;
        this.absorbedDamage  += actualAbsorbAmount;

        return actualAbsorbAmount;
    }
}



// MovementGenerator
// -----------------------------------------------------------------------
//
// This is the interface which all movement generators need to implement.

export interface MovementGenerator {
    initialize(): void;
    interrupt(): void;
}
