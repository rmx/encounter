import * as PT from '../Pure/Task';
import { lookupEntity, Party, controllingPlayer, Spell, Aura, playerParty }
    from '../Pure/Game';
import { State } from '../Game/State';
import { CombatEvent, Kill } from '../Pure/Game/Events';
import { WorldObject, isAlive } from '../Game/Entities/WorldObject';


export function
processEvent
( state : State
, event : Event
, task  : PT.Task<any>
, party : Party
): void {
    if (task.state instanceof PT.KillCreature) {
        KillCreature.processEvent(state, event, task, party);

    } else if (task.state instanceof PT.KillParties) {
        // No need to process the event. All the tasks logic is
        // implemented in its completely 'isCompleted' function.

    } else {
        throw new Error('processTaskEvent: Unknown task');
    }
}


export function
isCompleted
( state : State
, task  : PT.Task<any>
, party : Party
): boolean {
    if (task.state instanceof PT.KillCreature) {
        return KillCreature.isCompleted(task);

    } else if (task.state instanceof PT.KillParties) {
        return KillParties.isCompleted(state, party);

    } else {
        throw new Error("taskCompleted: Unknown task");
    }
}




export module KillCreature {

    export function
    processEvent
    ( state : State
    , event : any // Event -- TS 1.6
    , task  : PT.Task<PT.KillCreature>
    , party : Party
    ): void {
        var taskState = <PT.KillCreature> task.state;

        if (event instanceof CombatEvent) {

            if (!(event.event instanceof Kill)) {
                return;
            }

            var caster = null;

            if (event.source instanceof Spell) {
                // FIXME: This cast should not be necessary, we're inside
                // a type guard here.
                var spell = <Spell>(event.source);
                caster = lookupEntity<WorldObject>(state, spell.casterId);
            }

            if (event.source instanceof Aura) {
                // FIXME: This cast should not be necessary, we're inside
                // a type guard here.
                var aura = <Aura>(event.source);
                caster = lookupEntity<WorldObject>(state, aura.casterId);
            }

            if (!caster)
                return;

            var creature = lookupEntity<WorldObject>(state, event.entityId);
            if (!creature)
                return;

            var player = controllingPlayer(state, caster);
            if (!player)
                return;

            var pParty = playerParty(state, player.accountId);
            if(!pParty)
                return;

            if (pParty.id === party.id &&
                creature.creatureId === task.state.creatureId) {

                taskState.count += 1;
            }
        }
    }

    export function
    isCompleted
    ( task : PT.Task<PT.KillCreature>
    ): boolean {
        return task.state.count >= task.state.requiredCount;
    }
}



export module KillParties {

    export function
    isCompleted
    ( state : State
    , party : Party
    ): boolean {

        var numAliveInSameParty  = 0;
        var numAliveInOtherParty = 0;

        state.parties.forEach(p => {
            p.players.forEach(player => {
                var entity = lookupEntity<WorldObject>(state, player.entityId);

                if (p.id == party.id) {
                    if (!entity || isAlive(entity)) {
                        numAliveInSameParty += 1;
                    }
                } else {
                    if (!entity || isAlive(entity)) {
                        numAliveInOtherParty += 1;
                    }
                }
            });
        });

        return (numAliveInOtherParty == 0 && numAliveInSameParty > 0);
    }
}
