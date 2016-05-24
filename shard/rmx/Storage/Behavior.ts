
import * as Avers from '../../vendor/avers';



export class Behavior {

    name   : string;
    script : string;

    static mk(name: string) {
        return Avers.mk(Behavior, {
            name: name,
        });
    }
}

Avers.definePrimitive(Behavior, 'name', 'Behavior');
Avers.definePrimitive(Behavior, 'script', 'class Behavior\nmodule.exports = Behavior');
