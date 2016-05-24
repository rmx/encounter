/// <reference path="./Reference.ts" />

module rmx.Storage {

    // -----------------------------------------------------------------------
    export class Waypoint {

        id       : string;
        position : number[];

        static mk(position: number[]): Waypoint {
            return Avers.mk<Waypoint>(Waypoint,
                { position : position
                }
            );
        }
    }

    Avers.definePrimitive(Waypoint, 'position');



    // -----------------------------------------------------------------------
    export class Path {

        id        : string;
        name      : string;
        waypoints : Waypoint[];

        static mk(name: string): Path {
            return Avers.mk<Path>(Path,
                { name      : name
                , waypoints : []
                }
            );
        }
    }

    Avers.defineCollection(Path, 'waypoints', Waypoint);



    // -----------------------------------------------------------------------
    export class PointOfInterest {

        id       : string;
        name     : string;
        position : number[];

        static mk(name: string, position: number[] ): PointOfInterest {
            return Avers.mk<PointOfInterest>(PointOfInterest,
                { name     : name
                , position : position
                }
            );
        }
    }

    Avers.definePrimitive(PointOfInterest, 'name', '');
    Avers.definePrimitive(PointOfInterest, 'position');



    // -----------------------------------------------------------------------
    export class TileInstance {

        id       : string;
        tile     : Reference;
        position : number[]; // vec3
        rotation : number;   // 1|2|3|4


        static mk(tile: Reference, position: number[], rotation: number): TileInstance {
            return Avers.mk<TileInstance>(TileInstance,
                { tile     : tile
                , position : position
                , rotation : rotation
                }
            );
        }
    }

    Avers.defineObject   (TileInstance, 'tile', Reference, {});
    Avers.definePrimitive(TileInstance, 'position');
    Avers.definePrimitive(TileInstance, 'rotation');



    // -----------------------------------------------------------------------
    export class Terrain {

        name             : string;
        tiles            : TileInstance[];
        paths            : Path[];
        pointsOfInterest : PointOfInterest[];
        skybox           : Reference;
        sound            : Reference;
        ambientLight     : string;
        sunLight         : string;
        sunPos           : number[];
        shadowDarkness   : number;


        static mk(name: string) {
            return Avers.mk<Terrain>(Terrain,
                { name : name
                }
            );
        }
    }

    Avers.definePrimitive (Terrain, 'name');
    Avers.defineObject    (Terrain, 'skybox', Reference, {});
    Avers.defineObject    (Terrain, 'sound', Reference, {});
    Avers.defineCollection(Terrain, 'tiles', TileInstance);
    Avers.defineCollection(Terrain, 'paths', Path);
    Avers.defineCollection(Terrain, 'pointsOfInterest', PointOfInterest);
    Avers.definePrimitive (Terrain, 'ambientLight', "0x666666");
    Avers.definePrimitive (Terrain, 'sunLight', "0xffffff");
    Avers.definePrimitive (Terrain, 'sunPos', [70,70,100]);
    Avers.definePrimitive (Terrain, 'shadowDarkness', 0.6);
}
