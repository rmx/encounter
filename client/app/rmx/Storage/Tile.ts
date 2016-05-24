/// <reference path="./Model.ts" />

module rmx.Storage {

    // -----------------------------------------------------------------------
    export class Geometry {

        vertices : number[];
        faces    : number[];

        static mk(): Geometry {
            return Avers.mk<Geometry>(Geometry,
                { vertices : []
                , faces    : []
                }
            );
        }
    }

    Avers.definePrimitive (Geometry, 'vertices', []);
    Avers.definePrimitive (Geometry, 'faces', []);



    // -----------------------------------------------------------------------
    export class Tile {

        name     : string;
        size     : number[]; // vec3
        model    : Model;
        surface  : Geometry;

        static mk(name: string): Tile {
            return Avers.mk<Tile>(Tile,
                { name : name
                }
            );
        }
    }

    Avers.definePrimitive (Tile, 'name', '');
    Avers.definePrimitive (Tile, 'size', [1,1,1]);
    Avers.defineObject    (Tile, 'model', Model, {});
    Avers.defineObject    (Tile, 'surface', Geometry, {});
}
