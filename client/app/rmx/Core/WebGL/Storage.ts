/// <reference path="../../../ext/gl-matrix.ts" />
/// <reference path="../../../ext/three.d.ts" />

/// <reference path="../../WebGL/Scene.ts" />
/// <reference path="../../WebGL/RenderObject.ts" />

module rmx.Core.WebGL {

    // This module containes render functions of object which are defined in
    // the rmx.Storage module. The naming convention is: 'renderStorage<TypeName>'.


    import Scene             = rmx.WebGL.Scene;
    import renderObject      = rmx.WebGL.renderObject;
    import updateModelMatrix = rmx.WebGL.updateModelMatrix;
    import staticVisual      = rmx.WebGL.staticVisual;


    var tileRotationCenter = _.once(function() {
        return vec3.fromValues(1, 1, 1);
    });


    export function
    tileModel(tileId: string): Computation<rmx.Storage.Model> {
        return rmx.data.findById<rmx.Storage.Tile>(tileId).fmap(entity => {
            return entity.content.model;
        });
    }

    export function
    modelModel(modelId: string): Computation<rmx.Storage.Model> {
        return rmx.data.findById<rmx.Storage.Model>(modelId).fmap(entity => {
            return entity.content;
        });
    }


    export function
    renderStorageTileInstance
    ( scene : Scene
    , ti    : rmx.Storage.TileInstance
    ): void {
        var ro = renderObject(scene, 'tileInstance/' + ti.id);

        var rotZ = 0.5 * ti.rotation * Math.PI;
        updateModelMatrix(ro, <any>ti.position, rotZ, tileRotationCenter());
        ro.model = tileModel(ti.tile.objectId).get(null);

        ro.selectable = true;
    }


    export function
    renderStorageWaypoint
    ( scene : Scene
    , wp    : rmx.Storage.Waypoint
    ): void {
        var ro = renderObject(scene, 'waypoint/' + wp.id);

        updateModelMatrix(ro, <any>wp.position, 0);

        staticVisual(scene, ro, () => {
            var cube_geometry = new THREE.BoxGeometry(0.5, 0.5, 0.5);
            var cube_material = new THREE.MeshLambertMaterial({ color: 0xffdd44 });
            return new THREE.Mesh(cube_geometry, cube_material);
        });
    }


    export function
    renderStoragePointOfInterest
    ( scene : Scene
    , poi   : rmx.Storage.PointOfInterest
    ): void {
        var ro = renderObject(scene, 'pointOfInterest/' + poi.id);

        updateModelMatrix(ro, <any>poi.position, 0);

        staticVisual(scene, ro, () => {
            var cube_geometry = new THREE.BoxGeometry(0.5, 0.5, 0.5);
            var cube_material = new THREE.MeshLambertMaterial({ color: 0x44ddff });
            return new THREE.Mesh(cube_geometry, cube_material);
        });
    }


    export function
    renderStorageTerrain
    ( scene   : Scene
    , terrain : rmx.Storage.Terrain
    ): void {
        terrain.tiles.forEach(obj => {
            renderStorageTileInstance(scene, obj);
        });

        terrain.paths.forEach(path => {
            path.waypoints.forEach(obj => {
                renderStorageWaypoint(scene, obj);
            });
        });

        terrain.pointsOfInterest.forEach(poi => {
            renderStoragePointOfInterest(scene, poi);
        });
    }


    export function
    renderStorageSkybox
    ( scene    : Scene
    , skyboxId : string
    , skybox   : rmx.Storage.Skybox
    ): void {
        var ro = renderObject(scene, 'skybox@' + skyboxId);
        ro.selectable = false;
        staticVisual(scene, ro, () => {
            return skyboxMesh(skybox);
        });
    }

    function skyboxMesh(skybox: rmx.Storage.Skybox): THREE.Mesh {
        var vertexShader = [
            "varying vec3 vWorldPosition;",

            "void main() {",
                "vec4 worldPosition = modelMatrix * vec4( position, 1.0 );",
                "vWorldPosition = worldPosition.xyz;",

                "gl_Position = projectionMatrix * modelViewMatrix * ",
                    "vec4( position, 1.0 );",
            "}"
        ];

        var fragmentShader = [
            "uniform samplerCube tCube;",
            "uniform float tFlip;",

            "varying vec3 vWorldPosition;",

            "void main() {",
                "gl_FragColor = textureCube( tCube, vec3(",
                    "tFlip * vWorldPosition.x, vWorldPosition.zy ) );",
            "}"
        ];

        var uniforms =
            { "tCube": { type: "t", value: null }
            , "tFlip": { type: "f", value: -1   }
            };

        var texture_urls =
            [ rmx.blobUrl(skybox.getTexture("px").blobId)
            , rmx.blobUrl(skybox.getTexture("nx").blobId)
            , rmx.blobUrl(skybox.getTexture("py").blobId)
            , rmx.blobUrl(skybox.getTexture("ny").blobId)
            , rmx.blobUrl(skybox.getTexture("pz").blobId)
            , rmx.blobUrl(skybox.getTexture("nz").blobId)
            ];

        THREE.ImageUtils.crossOrigin = 'anonymous';
        var texture = THREE.ImageUtils.loadTextureCube(texture_urls);

        uniforms["tCube"].value = texture;
        var material = new THREE.ShaderMaterial({
            fragmentShader: fragmentShader.join("\n"),
            vertexShader  : vertexShader.join("\n"),
            uniforms      : <any> uniforms,
            side          : THREE.BackSide
        });

        var cube = new THREE.BoxGeometry(10000, 10000, 10000);
        var mesh = new THREE.Mesh(cube, material);
        mesh.position.z = 999;

        return mesh;
    }
}
