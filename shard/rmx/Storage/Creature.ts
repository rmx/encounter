
import * as Avers from '../../vendor/avers';

import { Reference } from './Reference';
import { Range } from './Range';



export class Creature {

    name     : string;
    model    : Reference;
    skinId   : string;
    health   : Range;
    spells   : Reference[];
    behavior : Reference;
    faction  : string;

    static mk(name: string) {
        return Avers.mk(Creature,
            { name : name
            }
        );
    }
}

Avers.definePrimitive  (Creature, 'name', '');
Avers.defineObject     (Creature, 'model', Reference, {});
Avers.definePrimitive  (Creature, 'skinId', null);
Avers.defineObject     (Creature, 'health', Range, { min: 0, max: 100 });
Avers.defineCollection (Creature, 'spells', Reference);
Avers.defineObject     (Creature, 'behavior', Reference, {});
Avers.definePrimitive  (Creature, 'faction', '');
