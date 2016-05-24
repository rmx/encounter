module rmx.Storage {

    // XXX: Same structure as 'File'

    export class Texture {

        type   : string;
        blobId : string;

        static mk(type: string): Texture {
            return Avers.mk<Texture>(Texture,
                { type: type
                }
            );
        }
    }

    Avers.definePrimitive(Texture, 'type');
    Avers.definePrimitive(Texture, 'blobId');
}

