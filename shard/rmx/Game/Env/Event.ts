
import * as Event from '../../Pure/Game/Events';
import { CombatEvent, PowerRegenEvent, EntityMutationEvent, RelocationEvent } from '../../Pure/Game/Events';



export function mk() {
    var env: any = {};


    env.eventType = function(ev: Event): string {
        if (ev instanceof CombatEvent) {
            return 'combat-event';

        } else if (ev instanceof PowerRegenEvent) {
            return 'power-regen';

        } else if (ev instanceof EntityMutationEvent) {
            return 'entity-mutation';

        } else if (ev instanceof RelocationEvent) {
            return 'relocation';

        } else {
            throw Error("event::eventType: Unknown event");
        }
    };


    return env;
}
