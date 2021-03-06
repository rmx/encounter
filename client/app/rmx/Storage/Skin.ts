/// <reference path="./Texture.ts" />

module rmx.Storage {

    export class Skin {

        name     : string;
        textures : Texture[];


        static mk(name: string): Skin {
            return Avers.mk<Skin>(Skin,
                { name: name
                }
            );
        }
    }

    Avers.definePrimitive (Skin, 'name');
    Avers.defineCollection(Skin, 'textures', Texture);
}

