

import * as J from '../../Pure/JSON';


// Elevation
// ------------
//
// Describes the elevation of the entity wrt to the ground. This is managed
// by the server (because it has effects on the game logic) and propagated
// to clients through SMSG_ENTITY_UPDATE messages.
//
// In the beginning we can expose script functions to change the elevation,
// but eventually it should be done through auras.

export enum Elevation
    { Submerged
      // ^ Only traces are visible on the surface (ie. what was visible in the
      // Ouro encounter). Entity is not targettable.

    , Ground
      // ^ Default. Consider a better name?

    , Levitating
      // ^ A fixed offset from the ground (0.5m?). Melee attack still possible.
      // Normal walking animations, but can walk across water etc.

    , Flying
      // ^ Flying high above the ground. Different animation stack. Only ranged
      // attacks work.
      //
      // TODO: What should be the vertical offset? Some models already include
      // a vertical offset in the animations.
    };



// ElevationTransition
// --------------------
//
// Because changes in elevation take a while, the transitions must be explicitly
// modeled in the game state. If an entity wants to transition from one elevation
// to another, we store the time when the transition was initiated and the desired
// target elevation. This gives the client time to play any necessary animations.
//
// The elevation transition is also propagated to clients via SMSG_ENTITY_UPDATE
// messages.
//
// One example of a game logic effect of a elevation transition is that if there
// is one active then the entity can not move.

export class ElevationTransition {
    constructor
      ( public initiatedAt : number
      , public elevation   : Elevation
      ) {}

    static parseJSON(json): ElevationTransition {
        return new ElevationTransition
            ( J.number(json, 'initiatedAt')
            , J.enumFromJSON(Elevation, json.elevation)
            );
    }

    toJSON() {
        return { initiatedAt : this.initiatedAt
               , elevation   : J.enumToJSON(Elevation, this.elevation)
               };
    }
}
