/// <reference path="../../ext/gl-matrix.ts" />
/// <reference path="../../ext/underscore.ts" />

/// <reference path="./BoundingBox.ts" />
/// <reference path="./Ids.ts" />
/// <reference path="./Navmesh.ts" />
/// <reference path="../Storage/Types.ts" />

// FIXME: Terrain shouldn't depend on Game.
/// <reference path="./Game.ts" />



module rmx.Pure {

    // Errors
    // -----------------------------------------------------------------------

    function notAnIntError(what) {
        return new Error("" + what + " is not an integer");
    }



    var offsetZero: Vec3 = vec3.fromValues(0, 0, 0);
    var offsetOne:  Vec3 = vec3.fromValues(1, 1, 1);


    export enum TileInstanceRotation {
        R0 = 0, R90 = 1, R180 = 2, R270 = 3
    };

    export var TILE_SIZE: number         = 2.0;
    var TILE_HALF_SIZE: number    = 1.0;

    var tileHalfSize:    Vec3 = vec3.fromValues( TILE_HALF_SIZE,  TILE_HALF_SIZE,  TILE_HALF_SIZE);
    var tileHalfSizeNeg: Vec3 = vec3.fromValues(-TILE_HALF_SIZE, -TILE_HALF_SIZE, -TILE_HALF_SIZE);


    function isInteger(x: number): boolean {
        return (x - Math.floor(x)) === 0;
    }

    // Network message types
    // -----------------------------------------------------------------------

    export interface TerrainPositionJSON {
        tname: EntityId;
        ti: string;
        t: number;
        bc0: number;
        bc1: number;
        bc2: number;
        h0: number;
    }

    export interface TerrainTileInstanceJSON {
        id        : string;
        tileId    : string;
        position  : number[];
        rotation  : number;
    }

    export interface TerrainJSON {
        id        : EntityId;
        tiles     : { [id: string]: rmx.Storage.Tile };
        instances : TerrainTileInstanceJSON[];
    }


    // Actual classes
    // -----------------------------------------------------------------------

    export class TerrainPosition {

        // Cache for the computed position in world coordinates. You must
        // use 'getWorldCoordinates' to access it, that function ensures
        // that the position is up-to-date.

        worldPos : Vec3;


        constructor
          ( public terrainId      : EntityId = null
            // ^ The terrain in which the position is located.

          , public tileInstanceId : string = null
            // ^ The tile instance (identified by its id).

          , public triangle       : number = null
            // ^ The face within the tile instance.

          , public baryCoord      : Vec3 = vec3.create()
            // ^ Barycentric coordinates within that face.

          , public heading        : number = null
            // ^ Rotation around the Z-axis. Maybe we should put it into
            // a separate component, eg. Rotation.
          ) {
            this.worldPos = vec3.create();
        }

        clone() {
            return new TerrainPosition
                ( this.terrainId
                , this.tileInstanceId
                , this.triangle
                , vec3.clone(this.baryCoord)
                , this.heading
                );
        }

        static parseJSON(json: any): TerrainPosition {
            return new TerrainPosition
                ( json.tname
                , json.ti
                , json.t
                , vec3.fromValues(json.bc0, json.bc1, json.bc2)
                , json.h0
                );
        }

        toJSON(): TerrainPositionJSON {
            return { tname : this.terrainId
                   , ti    : this.tileInstanceId
                   , t     : this.triangle
                   , bc0   : this.baryCoord[0]
                   , bc1   : this.baryCoord[1]
                   , bc2   : this.baryCoord[2]
                   , h0    : this.heading
                   };
        }

        eq(other: TerrainPosition): boolean {
            return this.terrainId      === other.terrainId
                && this.tileInstanceId === other.tileInstanceId
                && this.triangle       === other.triangle
                && vec3.dist(this.baryCoord, other.baryCoord) < 0.00001;
        }
    }


    // getWorldCoordinates
    // -----------------------------------------------------------------------
    //
    // TODO: This should take IState instead of the specific terrain and do
    // the entity lookup itself.

    export function
    getWorldCoordinates(terrain: Terrain, p: TerrainPosition): Vec3 {
        if (p.tileInstanceId) {
            var ti = terrain.tileInstances[p.tileInstanceId];
            if (ti) {
                ti.getWorldCoordinates(p, p.worldPos);
                return p.worldPos;
            }
        }
    }


    // setHeadingTarget
    // -----------------------------------------------------------------------
    //
    // Set the heading so that it is looking into the direction of the target.

    export function
    setHeadingTarget(terrain: Terrain, p: TerrainPosition, target: Vec3): void {
        var eye = getWorldCoordinates(terrain, p);

        var dx = target[0] - eye[0];
        var dy = target[1] - eye[1];

        p.heading = Math.atan2(dy, dx);
    }


    // terrainPositionAt
    // -----------------------------------------------------------------------
    //
    // Map world coordinates to a terrain position. Use this when you need to
    // create a new 'TerrainPosition'. If you already have an instance, use
    // 'relocateTerrainPosition' instead.

    export function
    terrainPositionAt(terrain: Terrain, x: Vec3): TerrainPosition {
        var p = new TerrainPosition;
        if (relocateTerrainPosition(p, terrain, x)) {
            return p;
        }
    }


    // relocateTerrainPosition
    // -----------------------------------------------------------------------
    //
    // Attempt to relocate a terrain position to the given terrain and vector
    // (in world coordinates). Returns true if it was successful (meaning the
    // given world coordinates map to a valid position in the terrain).

    export function
    relocateTerrainPosition(p: TerrainPosition, terrain: Terrain, x: Vec3): boolean {
        var cells: Vec3[] = cellsAroundPoint(x);
        var len: number = cells.length;
        for (var i: number = 0; i < len; i++) {
            var tileInstance = tileInstanceAtCell(terrain, cells[i]);
            if (tileInstance && tileInstance.project(p, x)) {
                p.terrainId = terrain.id;

                return true;
            }
        }

        return false;
    }


    // moveTerrainPosition
    // -----------------------------------------------------------------------
    //
    // Move the terrain position by a delta. Returns true if the move was
    // successful and the given terrain position has been updated.

    export function
    moveTerrainPosition(p: TerrainPosition, terrain: Terrain, delta: Vec3): boolean {
        var oldPos = getWorldCoordinates(terrain, p);
        var newPos = vec3.add(vec3.create(), oldPos, delta);

        return relocateTerrainPosition(p, terrain, newPos);
    }

    export interface TerrainIssue {
        cell : Vec3;
        // ^ The cell in which there is an issue.
    }

    export class Terrain {

        cellMap       : Map<string, TerrainTileInstance>;
        // ^ The cell map keeps track of which cells are occupied.

        navmesh       : HalfEdgeMesh;

        tileMap       : { [id: string]: TerrainTile };
        tileInstances : { [id: string]: TerrainTileInstance };

        issues        : TerrainIssue[];
        // ^ A list of issues (such as a cell conflict) which were encountered
        // during construction of the terrain.


        constructor
        ( public id : EntityId
          // ^ The entity Id of the terrain.

        , public resourceId : string
          // ^ The resource Id from which the terrain was created. It is used
          // on the client to load the correct skybox and ambient sound.

        ,        tileMap   : { [id: string]: rmx.Storage.Tile }
          // ^ The storage tiles which are used by the instances.

        ,        instances : TerrainTileInstanceJSON[]
          // ^ The tile instances in JSON format.
        ) {
            this.findHalfEdgeVertexAt = this.findHalfEdgeVertexAt.bind(this);
            this.findHalfEdgeAt       = this.findHalfEdgeAt.bind(this);

            this.cellMap       = new Map<string, TerrainTileInstance>();
            this.navmesh       = new HalfEdgeMesh;
            this.tileInstances = {};
            this.tileMap       = {};
            this.issues        = [];

            createTileInstances(this, tileMap, instances);
        }

        static parseJSON(json: any): Terrain {
            return new Terrain
                ( json.id
                , json.resourceId
                , json.tiles
                , json.instances
                );
        }

        toJSON(): any {
            var tiles = {};
            for (var id in this.tileMap) {
                tiles[id] = this.tileMap[id].toJSON();
            }

            var instances = [];
            for (var tiId in this.tileInstances) {
                instances.push(this.tileInstances[tiId].toJSON());
            }

            return { id         : this.id
                   , resourceId : this.resourceId
                   , tiles      : tiles
                   , instances  : instances
                   };
        }

        findTileById(id: string): TerrainTile {
            var tile = this.tileMap[id];
            if (tile) {
                return tile;
            } else {
                throw new Error("Tile " + id + " not found");
            }
        }

        addTileInstance(id: string, tile: TerrainTile, grid_pos: Vec3, rotation: TileInstanceRotation): void {
            var tileInstance = new TerrainTileInstance(id, tile, grid_pos, rotation);

            var conflicts = cellConflicts(this, tileInstance);
            if (conflicts.length > 0) {
                // console.info('Cell conflict when trying to add instance', id);

                conflicts.forEach(cell => {
                    this.issues.push({ cell: cell });
                });

            } else if (this.tileInstances[id] != null) {
                console.warn("Tile instance with ID " + id + " already exists");

            } else {
                this.tileInstances[id] = tileInstance;
                markCellsUsed(this, tileInstance);
            }
        }

        removeTileInstance(id: string): void {
            var tileInstance = this.tileInstances[id];
            if (tileInstance == null) {
                console.warn("Tile with ID " + id + " does not exists");

            } else {
                markCellsFree(this, tileInstance);
                delete this.tileInstances[id];
            }
        }

        getTerrainPosition(tile_id: number, local_x: number, local_y: number): TerrainPosition {
            var tile_instance = this.getTileInstanceByIndex(tile_id);
            var pos = new TerrainPosition;

            if (tile_instance && tile_instance.projectLocalCoordinates(pos, local_x, local_y)) {
                pos.terrainId = this.id;
                return pos;
            } else {
                return null;
            }
        }

        getTileInstanceByIndex(tile_index: number): TerrainTileInstance {
            var tile_id = Object.keys(this.tileInstances)[tile_index];
            var tile_instance = this.tileInstances[tile_id];

            return tile_instance;
        }

        toString(): string {
            var result: string = "";
            result = result + ("tile instances: " + (Object.keys(this.tileInstances).length));

            for (var id in this.tileInstances) {
                var ti = this.tileInstances[id];
                result += "\n instance " + id + ": " +
                    "pos = (" + ti.pos[0] + ", " + ti.pos[1] + ", " + ti.pos[2] + "), " +
                    "rot = " + ti.rotation;
            }
            return result;
        }

        forEachTileInstance(callback: (instance: TerrainTileInstance)=>void): void {
            for (var id in this.tileInstances) {
                var tileInstance = this.tileInstances[id];
                callback(tileInstance);
            }
        }

        findHalfEdgeAt(begin: Vec3, end: Vec3): HalfEdgeEdge {
            var cells = cellsAroundPoint(begin);
            var cellsLen: number = cells.length;
            for (var i: number = 0; i < cellsLen; i++) {
                var tile = tileInstanceAtCell(this, cells[i]);
                if (tile != null) {
                    var result: HalfEdgeEdge = tile.findHalfEdgeAt(begin, end);
                    if (result !== null) {
                        return result;
                    }
                }
            }

            return null;
        }

        findHalfEdgeVertexAt(vertex: Vec3): HalfEdgeVertex {
            var cells = cellsAroundPoint(vertex);
            var cellsLen: number = cells.length;
            for (var i: number = 0; i < cellsLen; i++) {
                var tile = tileInstanceAtCell(this, cells[i]);
                if (tile != null) {
                    var result: HalfEdgeVertex = tile.findHalfEdgeVertexAt(vertex);
                    if (result !== null) {
                        return result;
                    }
                }
            }

            return null;
        }

        findHalfEdgeFace(pos: TerrainPosition): HalfEdgeFace {
            var ti = this.tileInstances[pos.tileInstanceId];
            if (ti) {
                return ti.findHalfEdgeFace(pos);
            }
        }
    }


    export function
    createTileInstances
    ( terrain   : Terrain
    , tileMap   : { [id: string]: rmx.Storage.Tile }
    , instances : TerrainTileInstanceJSON[]
    ): void {
        // Create TerrainTile's which the terrain doens't have cached yet.
        for (var tileId in tileMap) {
            if (!terrain.tileMap[tileId]) {
                var tile = tileMap[tileId]
                  , size = vec3.clone(tile.size);

                terrain.tileMap[tileId] = new TerrainTile(tileId, tile.surface, size);
            }
        }

        instances.forEach(json => {
            terrain.addTileInstance
                ( json.id
                , terrain.findTileById(json.tileId)
                , vec3.clone(json.position)
                , json.rotation
                );
        });

        rebuildNavmesh(terrain);
    }


    export function
    rebuildNavmesh(terrain: Terrain): void {
        terrain.navmesh = new HalfEdgeMesh;
        terrain.forEachTileInstance(tileInstance => {
            tileInstance.insertNavigationMesh(terrain);
        });
        terrain.navmesh.normalizeAllVertices();
        terrain.navmesh.createStaticMesh();
    }

    function mkProjectionMatrix(pos: Vec3, rot: TileInstanceRotation) {
        var mat: Mat4 = mat4.create();

        mat4.identity (mat);
        mat4.translate(mat, mat, pos);
        mat4.translate(mat, mat, tileHalfSize);
        mat4.rotateZ  (mat, mat, 0.5 * rot * Math.PI);
        mat4.translate(mat, mat, tileHalfSizeNeg);

        return mat;
    }

    export class TerrainTileInstance {

        posWorld     : Vec3;
        matProj      : Mat4;
        matUnproj    : Mat4;
        navMeshFaces : HalfEdgeFace[];
        offsetMin    : Vec3;
        offsetMax    : Vec3;


        constructor
        ( public id       : string
        , public tile     : TerrainTile
        , public pos      : Vec3
        , public rotation : TileInstanceRotation
        ) {
            if (!isInteger(this.pos[0])) {
                throw notAnIntError("terrain tile position");
            }
            if (!isInteger(this.pos[1])) {
                throw notAnIntError("terrain tile position");
            }
            if (!isInteger(this.pos[2])) {
                throw notAnIntError("terrain tile position");
            }

            this.posWorld  = vec3.copy(vec3.create(), this.pos);

            this.matProj   = mkProjectionMatrix(this.posWorld, this.rotation);
            this.matUnproj = mat4.invert(mat4.create(), this.matProj);

            this.navMeshFaces = [];
            var sx = this.tile.size[0] - 1;
            var sy = this.tile.size[1] - 1;
            var sz = this.tile.size[2] - 1;
            var dx = 0;
            var dy = 0;
            var dz = sz;
            switch (this.rotation) {
            case TileInstanceRotation.R0:
                dx =  sx;
                dy =  sy;
                break;

            case TileInstanceRotation.R90:
                dx = -sy;
                dy =  sx;
                break;

            case TileInstanceRotation.R180:
                dx = -sx;
                dy = -sy;
                break;

            case TileInstanceRotation.R270:
                dx =  sy;
                dy = -sx;
                break;

            default:
                throw Error("Unknown rotation: " + this.rotation);
            }

            this.offsetMin = vec3.fromValues(Math.min(0, dx), Math.min(0, dy), Math.min(0, dz));
            this.offsetMax = vec3.fromValues(Math.max(0, dx), Math.max(0, dy), Math.max(0, dz));
        }

        toJSON(): TerrainTileInstanceJSON {
            return {
                id: this.id,
                tileId: this.tile.id,
                position: [this.pos[0], this.pos[1], this.pos[2]],
                rotation: this.rotation
            };

        }

        private static project_temp: Vec3 = vec3.create();
        project(tpos: TerrainPosition, x: Vec3): boolean {
            vec3.transformMat4(TerrainTileInstance.project_temp, x, this.matUnproj);

            if (this.tile.project(tpos, TerrainTileInstance.project_temp)) {
                tpos.tileInstanceId = this.id;
                return true;
            } else {
                return false;
            }
        }

        projectLocalCoordinates(pos: TerrainPosition, local_x: number, local_y: number): boolean {
            var local_vec = vec3.create();
            local_vec[0] = local_x * this.tile.size[0];
            local_vec[1] = local_y * this.tile.size[1];
            local_vec[2] = 0.5;

            if (this.tile.project(pos, local_vec)) {
                pos.tileInstanceId = this.id;
                return true;
            } else {
                return false;
            }
        }

        getWorldCoordinates(p: TerrainPosition, result: Vec3): void {
            this.tile.getWorldCoordinates(p, result);
            vec3.transformMat4(result, result, this.matProj);
        }

        insertNavigationMesh(terrain: Terrain): void {

            // Convert tile vertices into proper HalfEdgeVertex objects.
            // The code automatically creates half edge vertices if they
            // don't exist yet.
            var tolerance = 1e-4;
            var halfEdgeVertices = this.tile.vertices.map(local => {
                var position = vec3.create();
                vec3.transformMat4(position, local, this.matProj);

                var vertex = terrain.navmesh.findVertexAt(position, tolerance);
                if (vertex == null) {
                    vertex = terrain.navmesh.createVertex(position);
                }

                return vertex;
            });


            // For each tile face, create one face in the half edge mesh.
            this.tile.projectTris.forEach(tri => {
                var v0 = halfEdgeVertices[tri[0]];
                var v1 = halfEdgeVertices[tri[1]];
                var v2 = halfEdgeVertices[tri[2]];
                var face = terrain.navmesh.newFace([v0, v1, v2]);

                this.navMeshFaces.push(face);
                terrain.navmesh.stitchFaceBorderNoSubdivision(face, terrain.findHalfEdgeAt);

                face.setData("terrain",      terrain);
                face.setData("tileInstance", this);
                face.setData("tileTriangle", tri);
            });
        }

        findHalfEdgeAt(begin: Vec3, end: Vec3): HalfEdgeEdge {
            var faces: HalfEdgeFace[] = this.navMeshFaces;
            var facesLen: number = faces.length;
            for (var j = 0; j < facesLen; j++) {
                var face = faces[j];
                var result: HalfEdgeEdge = null;
                face.forEachEdge(function (e) {
                    var d1: number = vec3.squaredDistance(end, e.end.pos);
                    var d2: number = vec3.squaredDistance(begin, e.begin.pos);
                    if (d1 + d2 <= 1e-6) {
                        result = e;
                    }
                });
                if (result !== null) {
                    return result;
                }
            }

            return null;
        }

        findHalfEdgeVertexAt(vertex: Vec3): HalfEdgeVertex {
            var faces: HalfEdgeFace[] = this.navMeshFaces;
            var facesLen: number = faces.length;
            for (var j = 0; j < facesLen; j++) {
                var face = faces[j];
                var result: HalfEdgeVertex = null;
                face.forEachVertex(function (v) {
                    if (vec3.squaredDistance(v.pos, vertex) <= 1e-6) {
                        result = v;
                    }
                });
                if (result !== null) {
                    return result;
                }
            }

            return null;
        }

        findHalfEdgeFace(pos: TerrainPosition): HalfEdgeFace {
            return this.navMeshFaces[pos.triangle];
        }
    }



    function flatArrayToVectorArray(flatArray: number[]): Vec3[] {
        var inputLength   = flatArray.length;
        var index: number = 0;
        var ret: Vec3[]   = [];

        while (index + 2 < inputLength) {
            ret.push(vec3.fromValues(
                flatArray[index + 0],
                flatArray[index + 1],
                flatArray[index + 2]));
            index += 3;
        }

        return ret;
    }


    var insideTriMin: number = -0.001;
    var insideTriMax: number =  1.001;


    export function isInsideTriangle(b: Vec3): boolean {
        return b[0] >= insideTriMin && b[0] <= insideTriMax &&
               b[1] >= insideTriMin && b[1] <= insideTriMax &&
               b[2] >= insideTriMin && b[2] <= insideTriMax;
    }

    export function baryToCartesian(v0: Vec3, v1: Vec3, v2: Vec3, b: Vec3, result: Vec3): void {
        result[0] = b[0] * v0[0] + b[1] * v1[0] + b[2] * v2[0];
        result[1] = b[0] * v0[1] + b[1] * v1[1] + b[2] * v2[1];
        result[2] = b[0] * v0[2] + b[1] * v1[2] + b[2] * v2[2];
    }

    export function baryFromXY(v0: Vec3, v1: Vec3, v2: Vec3, x: Vec3, result: Vec3): void {
        var a     = v0[0] - v2[0];
        var b     = v1[0] - v2[0];
        var c     = v0[1] - v2[1];
        var d     = v1[1] - v2[1];
        var e     =  x[0] - v2[0];
        var f     =  x[1] - v2[1];

        var denom =  a * d - b * c;
        var nom0  =  d * e - b * f;
        var nom1  = -c * e + a * f;
        var b0    = nom0 / denom;
        var b1    = nom1 / denom;
        var b2    = 1.0 - b0 - b1;

        vec3.set(result, b0, b1, b2);
    }

    export class TerrainTile {

        vertices     : Vec3[];
        projectTris  : Vec3[];
        terrainType  : string[];


        constructor
        ( public id      : string
        , public surface : rmx.Storage.Geometry
        , public size    : Vec3
        ) {
            this.vertices     = flatArrayToVectorArray(surface.vertices);
            this.projectTris  = flatArrayToVectorArray(surface.faces);
            this.terrainType  = [];

            this.size         = size || vec3.fromValues(1, 1, 1);

            var terrainTypes  = /* surface.terrainType ||*/ [];
            for (var i = 0; i < terrainTypes.length; i++) {
                var terrainType: string = terrainTypes[i];
                this.terrainType.push(terrainType);
            }
            while (this.terrainType.length < this.projectTris.length) {
                this.terrainType.push("");
            }
        }

        toJSON(): rmx.Storage.Tile {
            return {
                id: this.id,
                name: null,
                model: null,
                surface: this.surface,
                size: [this.size[0], this.size[1], this.size[2]]
            };
        }

        project(tpos: TerrainPosition, x: Vec3): boolean {
            var tris = this.projectTris;
            var trisLen: number = tris.length;
            for (var i = 0; i < trisLen; i++) {
                if (this.projectTri(i, x, tpos)) {
                    return true;
                }
            }

            return false;
        }

        getWorldCoordinates(p: TerrainPosition, result: Vec3): void {
            this.barycentricToCartesian(this.projectTris[p.triangle], p.baryCoord, result);
        }

        private getVertexCoord(i: number): Vec3 {
            return this.vertices[i];
        }

        private barycentricToCartesian(face: Vec3, b: Vec3, result: Vec3): void {
            var v0 = this.getVertexCoord(face[0]);
            var v1 = this.getVertexCoord(face[1]);
            var v2 = this.getVertexCoord(face[2]);

            baryToCartesian(v0, v1, v2, b, result);
        }

        private cartesianXYToBarycentric(face: Vec3, x: Vec3, result: Vec3): void {
            var v0 = this.getVertexCoord(face[0]);
            var v1 = this.getVertexCoord(face[1]);
            var v2 = this.getVertexCoord(face[2]);

            baryFromXY(v0, v1, v2, x, result);
        }

        private static projectTri_temp: Vec3 = vec3.create();
        private projectTri(faceIndex: number, x, result: TerrainPosition): boolean {
            var face = this.projectTris[faceIndex];

            this.cartesianXYToBarycentric(face, x, TerrainTile.projectTri_temp);

            if (isInsideTriangle(TerrainTile.projectTri_temp)) {
                vec3.copy(result.baryCoord, TerrainTile.projectTri_temp);
                result.triangle = faceIndex;
                return true;
            } else {
                return false;
            }
        }
    }



    // cellConflicts
    // -----------------------------------------------------------------------
    //
    // Return a list of cells which are already occupied and therefore prevent
    // the given tile instance from being added to the terrain.

    function
    cellConflicts(terrain: Terrain, tileInstance: TerrainTileInstance): Vec3[] {
        var ret = [];

        forEachCellOfTile(tileInstance, offsetZero, cell => {
            if (terrain.cellMap.has(cellKey(cell))) {
                ret.push(cell);
            }
        });

        return ret;
    }


    // markCellsUsed
    // -----------------------------------------------------------------------
    //
    // Mark all cells which the tile instance occupies as used.

    function
    markCellsUsed(terrain: Terrain, tileInstance: TerrainTileInstance): void {
        forEachCellOfTile(tileInstance, offsetZero, cell => {
            terrain.cellMap.set(cellKey(cell), tileInstance);
        });
    }


    // markCellsFree
    // -----------------------------------------------------------------------
    //
    // Mark all cells which the tile instance occupies as free.

    function
    markCellsFree(terrain: Terrain, tileInstance: TerrainTileInstance): void {
        forEachCellOfTile(tileInstance, offsetZero, cell => {
            terrain.cellMap.delete(cellKey(cell));
        });
    }

    export function
    getTileNeighbors(terrain: Terrain, tileInstance: TerrainTileInstance): TerrainTileInstance[] {
        var neighbors: TerrainTileInstance[] = [];

        forEachCellOfTile(tileInstance, offsetOne, cell => {
            var neighbor = tileInstanceAtCell(terrain, cell);
            if (neighbor !== tileInstance && neighbor != null) {
                neighbors.push(neighbor);
            }
        });

        return _.uniq(neighbors);
    }

    function
    forEachCellInRange(cell_min: Vec3, cell_max: Vec3, callback: (cell: Vec3)=>void): void {
        for (var x: number = cell_min[0]; x <= cell_max[0]; x++) {
            for (var y: number = cell_min[1]; y <= cell_max[1]; y++) {
                for (var z: number = cell_min[2]; z <= cell_max[2]; z++) {
                    callback(vec3.fromValues(x, y, z));
                }
            }
        }
    }

    function
    forEachCellOfTile
    ( tileInstance : TerrainTileInstance
    , offset       : Vec3
    , callback     : (cell: Vec3) => void
    ): void {
        var cell_min = vec3.scale(vec3.create(), tileInstance.pos, 0.5);
        var cell_max = vec3.scale(vec3.create(), tileInstance.pos, 0.5);

        vec3.add(cell_min, cell_min, tileInstance.offsetMin);
        vec3.add(cell_max, cell_max, tileInstance.offsetMax);
        vec3.add(cell_max, cell_max, offset);
        vec3.subtract(cell_min, cell_min, offset);

        forEachCellInRange(cell_min, cell_max, callback);
    }


    function
    cellKey(cell: Vec3): string {
        return vec3.str(cell);
    }

    export function
    tileInstanceAtCell(terrain: Terrain, cell: Vec3): TerrainTileInstance {
        return terrain.cellMap.get(cellKey(cell));
    }


    // cellsAroundPoint
    // -----------------------------------------------------------------------
    //
    // Return a list of cells around the given point. Returns up to four cell
    // locations. They are guaranteed to be unique.

    function
    cellsAroundPoint(pos: Vec3): Vec3[] {
        var eps = 1e-5;

        return _.uniq(
            [ worldToCell(vec3.fromValues(pos[0] + eps, pos[1] + eps, pos[2]))
            , worldToCell(vec3.fromValues(pos[0] - eps, pos[1] + eps, pos[2]))
            , worldToCell(vec3.fromValues(pos[0] + eps, pos[1] - eps, pos[2]))
            , worldToCell(vec3.fromValues(pos[0] - eps, pos[1] - eps, pos[2]))
            ]
        );
    }

    export function
    worldToCell(x: Vec3): Vec3 {
        return vec3.fromValues
            ( Math.floor(x[0] / TILE_SIZE)
            , Math.floor(x[1] / TILE_SIZE)
            , Math.floor(x[2] / TILE_SIZE)
            );
    }

    export function
    cellToWorld(x: Vec3): Vec3 {
        return vec3.fromValues
            ( x[0] * TILE_SIZE
            , x[1] * TILE_SIZE
            , x[2] * TILE_SIZE
            );
    }
}
