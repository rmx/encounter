
import * as Avers from '../../vendor/avers';



export class File {

    name   : string;
    blobId : string;

    static mk(name: string): File {
        return Avers.mk<File>(File,
            { name : name
            }
        );
    }
}

Avers.definePrimitive(File, 'name');
Avers.definePrimitive(File, 'blobId');
