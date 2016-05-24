
import * as Storage from '../Storage';
import { State } from '../Game/State';
import { interruptSpellcast } from '../Game/Spell';
import { broadcastMessage, uniqueId, lookupResource, loadScript, NoScript } from '../Game';
import { WorldObject, stopMoving } from '../Game/Entities/WorldObject';
import { SMSG_FIELDS } from '../Game/Messages';
import { OldGlobal } from '../Game/Global';
import { toString } from '../Pure/String';

import { _ } from 'underscore';



export interface BehaviorId {}

export interface BehaviorScript {

    // The top behavior (in the stack) receives events.
    handleEvent ?: (e: Event) => void;

    // Suspend is called when a new behavior is added to the stack and
    // pushes this one down.
    suspend     ?: () => void;

    // Resume is called when the behavior bubbles up to the top of the
    // stack.
    resume      ?: () => void;

    // Stop is called when the behavior is removed from the stack.
    stop        ?: () => void;
}


export class Behavior {

    constructor
      ( public resourceId : string
      , public behaviorId : BehaviorId
      , public script     : BehaviorScript
      , public data       : any
        // ^ An optional field which the caller can use for whatever you
        // need. For example to track where the behavior came from.
      ) {}

    toString() {
        return toString(this, 'Behavior', 'behaviorId');
    }

    processEvent(e: Event): void {
        if (this.script.handleEvent) {
            this.script.handleEvent(e);
        }
    }

    suspend() {
        if (this.script.suspend) {
            this.script.suspend();
        }
    }

    resume() {
        if (this.script.resume) {
            this.script.resume();
        }
    }

    stop() {
        if (this.script.stop) {
            this.script.stop();
        }
    }
}


// lookupBehaviorResource
// -----------------------------------------------------------------------

function
lookupBehaviorResource(state: State, id: string): Storage.Behavior {
    return lookupResource<Storage.Behavior>(state, id, 'behavior');
}


// attachBehavior
// -----------------------------------------------------------------------
//
// Attach a behavior to the entity. If the entity already has an active
// behavior then that will be suspended and pushed down the stack.
//
// Returns an Id which can be used to remove the behavior from the stack.

export function
attachBehavior
( state      : State
, entity     : WorldObject
, resourceId : string
, data       : any
): BehaviorId {
    var id          = <BehaviorId> uniqueId(state)
      , source      = lookupBehaviorResource(state, resourceId).script
      , env         = new OldGlobal(state, entity, entity)
      , Script      = loadScript('behavior/' + resourceId, source, env)
      , newBehavior = new Behavior(resourceId, id, new (Script || NoScript), data)
      , topBehavior = entity.behaviorStack[0];

    if (topBehavior) {
        topBehavior.suspend();
    }

    entity.behaviorStack.unshift(newBehavior);

    var fields = { serverBehavior: true };
    broadcastMessage(state, SMSG_FIELDS(entity, fields));

    return id;
}


// removeBehavior
// -----------------------------------------------------------------------

export function
removeBehavior
( state      : State
, entity     : WorldObject
, behaviorId : BehaviorId
): void {
    var behavior = _.find(entity.behaviorStack, function(x) {
        return x.behaviorId === behaviorId;
    });

    if (behavior) {
        behavior.stop();

        var index = entity.behaviorStack.indexOf(behavior);
        entity.behaviorStack.splice(index, 1);

        if (index === 0) {
            var topBehavior = entity.behaviorStack[0];
            if (topBehavior != null) {
                topBehavior.resume();
            }
        }

        var fields = { serverBehavior: hasBehavior(entity) };
        broadcastMessage(state, SMSG_FIELDS(entity, fields));

        stopMoving(state, entity);
        interruptSpellcast(state, entity);
    }
}


// hasBehavior
// -----------------------------------------------------------------------

export function
hasBehavior(entity: WorldObject): boolean {
    return entity.behaviorStack.length > 0;
}


// XXX
export function
lookupBehaviorByResourceId(entity: WorldObject, resourceId: string): Behavior {
    return _.find(entity.behaviorStack, function(x) {
        return x.resourceId === resourceId;
    });
}
