/// <reference path="../../ext/underscore.ts" />
/// <reference path="./Texture.ts" />

module rmx.Storage {

    export class Skybox {

        name     : string;
        textures : Texture[];


        static mk(name: string): Skybox {
            function mkTexture(type) {
                var json = Avers.toJSON(Avers.mk(Texture, { type: 'px' }));
                json.id = type;
                return json;
            }

            var textures =
                [ mkTexture('px')
                , mkTexture('py')
                , mkTexture('pz')
                , mkTexture('nx')
                , mkTexture('ny')
                , mkTexture('nz')
                ];

            return Avers.mk<Skybox>(Skybox, { name: name, textures: textures });
        }

        getTexture(name: string): Texture {
            return _.find(this.textures, texture => {
                return texture.type == name;
            });
        }
    }

    Avers.definePrimitive (Skybox, 'name', 'New Skybox');
    Avers.defineCollection(Skybox, 'textures', Texture);
}
