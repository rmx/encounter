
import { EntityId } from '../../Pure/Ids';
import * as J from '../../Pure/JSON';
import { Aura, Spell } from '../../Pure/Game';


// Event
// -----------------------------------------------------------------------
//
// All events which may be emitted by the game.

export type Event
    = PowerRegenEvent
    | CombatEvent
    | EntityMutationEvent
    | RelocationEvent
    ;



// EntityMutationEvent
// -----------------------------------------------------------------------
//
// An entity mutation event is a change of a entity property or attribute.
// It is often a consequence of a combat action. But can also occur
// independently.
//
// This event is emitted only on the server. It is not sent or processed
// on the client (yet?), so there is no need for toJSON/parseJSON.

export class EntityMutationEvent {
    constructor
      ( public entityId : EntityId
      , public mutation : EntityMutation
      ) {}
}


export type EntityMutation
    = MaxHealthMutation
    | HealthMutation
    ;


export class MaxHealthMutation {}
export class HealthMutation {}



// RelocationEvent
// -----------------------------------------------------------------------
//
// Emitted whenever an entity changes its terrainPosition. This can be due
// to normal movement (either by the player or through its movePath), or
// because the entity was teleported to a new position (possibly to
// a different terrain).

export class RelocationEvent {
    constructor
        ( public entityId : EntityId
        ) {}
}



// Environment
// -----------------------------------------------------------------------
//
// Used a the source of a combat event when the source of the event is not
// an entity but the environment (tile, glue script etc).

export class Environment {
    toJSON() {
    }

    static parseJSON() {
        return new Environment;
    }
}




// Heal
// -----------------------------------------------------------------------

export class Heal {
    constructor
      ( public amount : number
      ) {}

    toJSON() {
        return { amount : this.amount };
    }

    static parseJSON(json) {
        return new Heal(json.amount);
    }
}



// Damage
// -----------------------------------------------------------------------

export class Damage {
    constructor
      ( public school   : string
      , public amount   : number
      , public absorbed : number
      , public overkill : number
      ) {}

    toJSON() {
        return { school   : this.school
               , amount   : this.amount
               , absorbed : this.absorbed
               , overkill : this.overkill
               };
    }

    static parseJSON(json) {
        return new Damage
            ( json.school
            , json.amount
            , json.absorbed
            , json.overkill
            );
    }
}



// Kill
// -----------------------------------------------------------------------
//
// Some entity killed another entity. Not used when the death was caused
// by environmental damage. This can be used to track killing blows.
//
// Usually emitted just after a Damage event because only damage can kill
// entities.

export class Kill {
    toJSON() {
    }

    static parseJSON() {
        return new Kill;
    }
}



// CombatEvent
// -----------------------------------------------------------------------
//
// A combat event is a direct consequence of a combat action (eg. a spell)
// which somebody (entity or environment) performs to a target (another
// entity).

export class CombatEvent {
    constructor
      ( public source : Environment | Spell | Aura
        // ^ What caused the event. If unknown, then the source is the
        // environment.

      , public entityId : EntityId
        // ^ The affected entity.

      , public event : Heal | Damage | Kill
        // ^ The event itself.
      ) {}

    toJSON() {
        return { source   : J.variantToJSON(this.source, combatEventSourceTypes)
               , entityId : this.entityId
               , event    : J.variantToJSON(this.event, combatEventTypes)
               };
    }

    static parseJSON(json): CombatEvent {
        return new CombatEvent
            ( J.variantFromJSON(json.source, combatEventSourceTypes)
            , J.string(json, 'entityId')
            , J.variantFromJSON(json.event, combatEventTypes)
            );
    }
}

var combatEventSourceTypes =
    { environment : Environment
    , spell       : Spell
    , aura        : Aura
    };

var combatEventTypes =
    { heal   : Heal
    , damage : Damage
    , kill   : Kill
    };



// PowerRegenEvent
// -----------------------------------------------------------------------
//
// Event emitted at fixed intervals (currently every 5 seconds) when
// entity power is generated.

export class PowerRegenEvent {
}
