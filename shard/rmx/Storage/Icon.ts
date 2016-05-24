
import * as Avers from '../../vendor/avers';



module rmx.Storage {

    export class Icon {

        name   : string;
        blobId : string;

        static mk(name: string) {
            return Avers.mk(Icon, { name: name });
        }
    }

    Avers.definePrimitive(Icon, 'name');
    Avers.definePrimitive(Icon, 'blobId');
}
