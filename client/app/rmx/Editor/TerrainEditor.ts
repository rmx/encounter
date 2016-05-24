/// <reference path="../Pure/PathFinder.ts" />

/// <reference path="../Storage/Types.ts" />

/// <reference path="../Core/Input.ts" />
/// <reference path="../Core/Bindings.ts" />
/// <reference path="../Core/InputSource.ts" />

/// <reference path="../Core/WebGL/Storage.ts" />
/// <reference path="../Core/WebGL/Pure.ts" />

/// <reference path="./ModelRenderer" />


module rmx.Editor {

    import terrainPositionAt   = rmx.Pure.terrainPositionAt;

    import KeyState            = rmx.Core.KeyState;
    import Input               = rmx.Core.Input;
    import Bindings            = rmx.Core.Bindings;
    import InputSource         = rmx.Core.InputSource;


    export module TerrainEditorActions {

        // Movement
        // -------------------------------------------------------------------


        var speed = 20; // meter / second.
        function moveBy(ctx: TerrainEditor, xv, yv, dt) {
            var h  = ctx.camera.heading
              , dx = dt * ( xv * Math.sin(h) + yv * Math.cos(h))
              , dy = dt * (-xv * Math.cos(h) + yv * Math.sin(h));

            ctx.cameraTarget[0] += dx;
            ctx.cameraTarget[1] += dy;
        }

        TerrainEditorActions["move-forward"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            moveBy(ctx, 0, +speed, keyState.totalTime / 1000);
        };

        TerrainEditorActions["move-backward"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            moveBy(ctx, 0, -speed, keyState.totalTime / 1000);
        };

        TerrainEditorActions["strafe-left"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            moveBy(ctx, -speed, 0, keyState.totalTime / 1000);
        };

        TerrainEditorActions["strafe-right"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            moveBy(ctx, +speed, 0, keyState.totalTime / 1000);
        };


        TerrainEditorActions["camera-zoom-in"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            ctx.camera.changeZoom(+1);
        };

        TerrainEditorActions["camera-zoom-out"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            ctx.camera.changeZoom(-1);
        };

        TerrainEditorActions["camera-rotate"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            var movement = ctx.input.pointerMovement(1);
            if (movement && movement[0] !== 0)  {
                var headingDiff = 2 * Math.PI * movement[0]
                  , pitchDiff   = Math.PI * movement[1];

                ctx.camera.changeHeading(headingDiff);
                ctx.camera.changePitch(pitchDiff);

            }
        };

        TerrainEditorActions["place"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            placeSelectedItem(ctx, keyState);
        };

        TerrainEditorActions["rotate-selected-tile"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.tileRotation = (ctx.tileRotation + 1) % 4;
            }
        };

        TerrainEditorActions["deselect-tile"] =
        (ctx : TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.selectTile(null);
            }
        };


        TerrainEditorActions["toggle-navmesh-rendering"] =
        (ctx: TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.showNavmesh = !ctx.showNavmesh;
            }
        };

        TerrainEditorActions["toggle-pathfinder"] =
        (ctx: TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.runPathfinder = !ctx.runPathfinder;
            }
        };

        TerrainEditorActions["z-plane-up"] =
        (ctx: TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.planeZ += 2;
            }
        };

        TerrainEditorActions["z-plane-down"] =
        (ctx: TerrainEditor, keyState: KeyState) => {
            if (!keyState.pressedAt) {
                ctx.planeZ -= 2;
            }
        };
    }

    function selectableFilter(obj: THREE.Object3D): boolean {
        var owner = rmx.WebGL.toRenderObject(obj);
        return owner && owner.selectable;
    }

    function
    placeSelectedItem
    ( editor   : TerrainEditor
    , keyState : KeyState
    ): void {
        var intersect = WebGL.intersectionAtPointer(editor.scene, selectableFilter);

        if (!intersect) {
            return;
        }

        var ro    = intersect.owner
          , face  = intersect.face
          , point = intersect.point;

        if (editor.selectedTile && !keyState.pressedAt) {
            var pos      = getCellCenter(ro.root, face, point)
              , position = [pos[0], pos[1], pos[2]]
              , rotation = editor.tileRotation
              , tile     = rmx.data.objIdReference(editor.selectedTile.objectId)
              , ti       = rmx.Storage.TileInstance.mk(Avers.toJSON(tile), position, rotation);
            rmx.data.pushItem(editor.terrain.tiles, ti);

        } else if (editor.selectedWaypoint && !keyState.pressedAt) {
            var waypoint = rmx.Storage.Waypoint.mk([point.x, point.y, point.z]);
            //FIXME: currently hardcoded to have one path
            if (editor.terrain.paths.length == 0) {
                var path = rmx.Storage.Path.mk("tmp");
                rmx.data.pushItem(editor.terrain.paths, path);
            }
            rmx.data.pushItem(editor.terrain.paths[0].waypoints, waypoint);

        } else if (editor.selectedPOI && !keyState.pressedAt) {
            var poi = rmx.Storage.PointOfInterest.mk('', [point.x, point.y, point.z]);
            rmx.data.pushItem(editor.terrain.pointsOfInterest, poi);

        } else if (!keyState.pressedAt) {
            intersect = WebGL.intersectionAtPointer(editor.scene, obj => {
                var owner = WebGL.toRenderObject(obj);
                return owner && owner.selectable && owner.id !== 'plane';
            });

            if (intersect) {
                var owner = intersect.owner;
                var ti2 = _.find(editor.terrain.tiles, x => {
                    return 'tileInstance/' + x.id === owner.id;
                });

                rmx.data.removeItem(editor.terrain.tiles, ti2);
            }
        }
    }


    function
    worldToCellCenter(pos: THREE.Vector3): Vec3 {
        return vec3.fromValues
            ( Math.floor(pos.x / 2) * 2
            , Math.floor(pos.y / 2) * 2
            , Math.floor(pos.z / 2) * 2
            );
    }

    export function
    getCellCenter
    ( object : THREE.Object3D
    , face   : THREE.Face3
    , point  : THREE.Vector3
    ): Vec3 {
        if (object && face && point) {
            var mat = new THREE.Matrix4().extractRotation(object.matrixWorld);
            var tmp = face.normal.clone().applyProjection(mat).multiplyScalar(0.1);
            var pos = new THREE.Vector3().addVectors(point, tmp);

            return worldToCellCenter(pos);
        } else {
            return vec3.fromValues(0,0,0);
        }
    }

    function mkPlane() {
        var geometry = new THREE.PlaneGeometry(120, 120, 60, 60);
        var material = new THREE.MeshBasicMaterial({ color: 0x555555, wireframe: true });
        return new THREE.Mesh(geometry, material);
    }

    function plane(scene: WebGL.Scene, z: number): WebGL.RenderObject {
        var ro = WebGL.renderObject(scene, 'plane');
        WebGL.staticVisual(scene, ro, mkPlane);
        WebGL.updateModelMatrix(ro, vec3.fromValues(0, 0, z), 0);
        ro.selectable = true;
        return ro;
    }


    function mkCube() {
        var geometry = new THREE.BoxGeometry(0.3, 0.3, 0.3);
        var material = new THREE.MeshBasicMaterial({ color: 0xFF0000 });
        return new THREE.Mesh(geometry, material);
    }

    function mkCellOutline() {
        var geometry = new THREE.BoxGeometry(2, 2, 2);
        var material = new THREE.MeshBasicMaterial({ color: 0xFF0000, wireframe: true });
        return new THREE.Mesh(geometry, material);
    }

    function cameraTarget(editor: TerrainEditor, scene: WebGL.Scene): WebGL.RenderObject {
        var ro = WebGL.renderObject(scene, 'cameraTarget');
        WebGL.staticVisual(scene, ro, mkCube);
        WebGL.updateModelMatrix(ro, editor.cameraTarget, 0);
        ro.selectable = false;
        return ro;
    }

    function
    mkCursorObject(scene: WebGL.Scene): WebGL.RenderObject {
        var ro = WebGL.renderObject(scene, 'cursorObject');
        ro.selectable = false;
        return ro;
    }

    export class TerrainEditor {

        input            : Input;
        bindings         : Bindings<TerrainEditor>;
        camera           : rmx.Game.Camera;

        scene            : WebGL.Scene;
        inputSource      : InputSource;

        // If a tile was selected from the toolbox, this is it.
        selectedTile     : rmx.data.Object<rmx.Storage.Tile>;
        selectedWaypoint : boolean;
        selectedPOI      : boolean;

        // The WebGL object rendered at the cursor position.
        tileRotation     : number;

        cameraTarget     : Vec3;
        // ^ The target at which the camera is looking. This can be moved
        // with the wasd keys.


        showNavmesh     : boolean;
        runPathfinder   : boolean;

        planeZ          : number;


        constructor
        ( public terrain : rmx.Storage.Terrain
        ) {
            this.scene        = new WebGL.Scene();
            this.input        = new Input;
            this.bindings     = new Bindings<TerrainEditor>(<any>TerrainEditorActions);
            this.camera       = new rmx.Game.Camera;

            this.scene        = new WebGL.Scene;
            this.inputSource  = new InputSource(this.input, this.bindings, this.scene.domElement);

            this.selectedWaypoint = false;

            this.tileRotation = 0;
            this.cameraTarget = vec3.fromValues(0, 0, 0);

            this.showNavmesh   = false;
            this.runPathfinder = false;

            this.planeZ        = 0;

            // The plane at elevation 0 to have something to place the tiles on.
            // Default bindings
            this.bindings.bindings['wheelup']     = { action: 'camera-zoom-in'       };
            this.bindings.bindings['wheeldown']   = { action: 'camera-zoom-out'      };
            this.bindings.bindings['button-1']    = { action: 'place'                };
            this.bindings.bindings['button-3']    = { action: 'camera-rotate'        };
            this.bindings.bindings['r']           = { action: 'rotate-selected-tile' };
            this.bindings.bindings['esc']         = { action: 'deselect-tile'        };

            this.bindings.bindings['w']           = { action: 'move-forward'         };
            this.bindings.bindings['s']           = { action: 'move-backward'        };
            this.bindings.bindings['a']           = { action: 'strafe-left'          };
            this.bindings.bindings['d']           = { action: 'strafe-right'         };

            this.bindings.bindings['[']           = { action: 'toggle-navmesh-rendering' };
            this.bindings.bindings[']']           = { action: 'toggle-pathfinder'    };

            this.bindings.bindings['+']           = { action: 'z-plane-up'    };
            this.bindings.bindings['-']           = { action: 'z-plane-down'  };
        }


        update(now: number): void {
            rmx.Core.dispatchInput(now, this.input, this.bindings, this);
            WebGL.startNextGeneration(this.scene);

            plane(this.scene, this.planeZ);

            cameraTarget(this, this.scene);
            applyCameraTo(this.camera, this.scene.camera, this.cameraTarget);
            rmx.Core.WebGL.renderStorageTerrain(this.scene, this.terrain);

            var intersect = WebGL.intersectionAtPointer(this.scene, selectableFilter);

            if (intersect) {
                var object = intersect.owner;
                if (object) {
                    var cursorObject = mkCursorObject(this.scene);
                    var pos = getCellCenter(object.root, intersect.face, intersect.point);
                    cursorObject.model = this.selectedTile ? this.selectedTile.content.model : null;
                    WebGL.updateModelMatrix
                        ( cursorObject
                        , <any>pos
                        , 0.5 * this.tileRotation * Math.PI
                        , vec3.fromValues(1, 1, 1)
                        );
                }
            }

            if (this.showNavmesh || this.runPathfinder) {
                var pureTerrain = toPureTerrain(this.terrain);

                if (this.showNavmesh) {
                    this.updateNavMesh(pureTerrain);
                }

                if (this.runPathfinder) {
                    this.runPathFinder(pureTerrain);
                }

                pureTerrain.issues.forEach(issue => {
                    var key = 'issue@' + vec3.str(issue.cell)
                      , ro  = WebGL.renderObject(this.scene, key);

                    var pos = vec3.clone(issue.cell);
                    vec3.mul(pos, pos, vec3.fromValues(2,2,2));
                    vec3.add(pos, pos, vec3.fromValues(1,1,1));

                    WebGL.updateModelMatrix(ro, pos, 0);
                    WebGL.staticVisual(this.scene, ro, mkCellOutline);
                    ro.selectable = false;
                });
            }
        }

        selectTile(tile: rmx.data.Object<rmx.Storage.Tile>): void {
            this.selectedTile     = tile;
            this.selectedWaypoint = false;
            this.selectedPOI      = false;
            rmx.data.startNextGeneration();
        }

        toggleWaypoint(): void {
            this.selectedWaypoint = !this.selectedWaypoint;
            this.selectedPOI      = false;
            this.selectedTile     = null;
            rmx.data.startNextGeneration();
        }

        togglePOI(): void {
            this.selectedWaypoint = false;
            this.selectedPOI      = !this.selectedPOI;
            this.selectedTile     = null;
            rmx.data.startNextGeneration();
        }


        // TODO: The server contains a generic computeWaypoints function which
        // computes a list of waypoints (Vec3) between two terrain positions.
        // Move it into rmx.Pure and reuse here.
        runPathFinder(pureTerrain: rmx.Pure.Terrain): void {
            if (this.terrain.paths.length < 1) {
                return;
            }

            var i;
            var len  = this.terrain.paths[0].waypoints.length;
            for (i = 1; i < len; i++) {
                // Waypoints, as 3D coordinates
                var spos = this.terrain.paths[0].waypoints[i-1].position;
                var epos = this.terrain.paths[0].waypoints[i].position;
                var spos_vec = vec3.fromValues(spos[0], spos[1], spos[2]);
                var epos_vec = vec3.fromValues(epos[0], epos[1], epos[2]);

                // Waypoints, as terrain positions
                // TODO: Why not store waypoints as terrain positions?
                var sppos = terrainPositionAt(pureTerrain, spos_vec);
                var eppos = terrainPositionAt(pureTerrain, epos_vec);

                // Mavmesh faces that contain the waypoints
                var sppos_face = pureTerrain.findHalfEdgeFace(sppos);
                var eppos_face = pureTerrain.findHalfEdgeFace(eppos);

                var pf = new rmx.Pure.PathFinder(pureTerrain, sppos_face, eppos_face,
                    sppos, eppos);
                var path_list = pf.compute();

                var pathObject = WebGL.renderObject(this.scene, 'pathSegment' + i);
                var geometry = new THREE.Geometry
                , color    = new THREE.Color(0xFFDD44)
                , material = new THREE.LineBasicMaterial({
                        color: 0xffdd44,
                        linewidth: 5
                    });

                path_list.reduce(function(v0, v1) {
                    geometry.vertices.push(new THREE.Vector3(v0[0], v0[1], v0[2] + 0.01));
                    geometry.vertices.push(new THREE.Vector3(v1[0], v1[1], v1[2] + 0.01));
                    geometry.colors.push(color);
                    geometry.colors.push(color);
                    return v1;
                });

                material.vertexColors = THREE.VertexColors;
                var mesh = new THREE.Line(geometry, material, THREE.LinePieces);
                WebGL.updateVisual(this.scene, pathObject, '' + this.scene.generationNumber, () => { return mesh; });
            }
        }

        updateNavMesh(terrain: rmx.Pure.Terrain): void {
            rmx.Core.WebGL.renderPureHalfEdgeMesh(this.scene, '', terrain.navmesh, this.scene.generationNumber);
        }
    }


    // toPureTerrain
    // ----------------------------------------------------------------------
    //
    // Create a Pure terrain out of a Storage terrain. May return null if not
    // all tile data is available locally.

    function
    toPureTerrain(terrain: rmx.Storage.Terrain): rmx.Pure.Terrain {
        var pureTerrain = new rmx.Pure.Terrain("arst", null, {}, []);

        var tiles = terrain.tiles.map((x) => {
            return rmx.data.findById<rmx.Storage.Tile>(x.tile.objectId).get(null);
        });

        tiles.forEach((obj, idx) => {
            if (obj !== null) {
                var x = obj.content;

                var ti = terrain.tiles[idx];
                var position = vec3.fromValues(ti.position[0], ti.position[1], ti.position[2]);
                var size = vec3.fromValues(x.size[0], x.size[1], x.size[2]);

                var terrainTile = new rmx.Pure.TerrainTile(obj.objectId, x.surface, size);

                pureTerrain.addTileInstance(ti.id, terrainTile, position, ti.rotation);
            }
        });

        rmx.Pure.rebuildNavmesh(pureTerrain);

        return pureTerrain;
    }
}
