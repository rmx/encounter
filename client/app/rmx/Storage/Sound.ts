module rmx.Storage {

    export class Sound {

        name   : string;
        blobId : string;


        static mk(name: string): Sound {
            return Avers.mk<Sound>(Sound, { name: name });
        }
    }

    Avers.definePrimitive(Sound, 'name', '');
    Avers.definePrimitive(Sound, 'blobId', null);
}
