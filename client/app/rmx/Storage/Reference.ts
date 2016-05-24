module rmx.Storage {

    export class Reference {

        objectId   : string;
        revisionId : number;
        path       : string;


        toString(): string {
            if (this.objectId) {
                var revId = this.revisionId ? '@' + this.revisionId : '';
                var path  = this.path       ? ':' + this.path       : '';

                return this.objectId + revId + path;
            }
        }

        static mk(objectId: string, revisionId: number, path: string): Reference {
            return Avers.mk<Reference>(Reference,
                { objectId   : objectId
                , revisionId : revisionId
                , path       : path
                }
            );
        }

        static parseString(ref: string): Reference {
            var m = ref.match(/(.*):(.*)/);

            if (m) {
                return Reference.mk(m[1], null, m[2]);
            } else {
                return Reference.mk(ref, null, '');
            }
        }
    }

    Avers.definePrimitive(Reference, 'objectId');
    Avers.definePrimitive(Reference, 'revisionId');
    Avers.definePrimitive(Reference, 'path');
}
