
import * as Avers from '../../vendor/avers';



module rmx.Storage {

    export class Shard {
        version : string;
        address : any[];
    }

    Avers.definePrimitive (Shard, 'version');
    Avers.definePrimitive (Shard, 'address');
}
