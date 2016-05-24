
import * as Avers from '../../vendor/avers';
import { Creature } from './Creature';



export class Class {

    name     : string;
    creature : Creature;

    static mk(name: string) {
        return Avers.mk(Class,
            { name : name
            }
        );
    }
}

Avers.definePrimitive (Class, 'name', '');
Avers.defineObject    (Class, 'creature', Creature, {});
