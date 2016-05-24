/// <reference path="../../ext/gl-matrix.ts" />

/// <reference path="./Mesh.ts" />
/// <reference path="./RenderObject.ts" />
/// <reference path="./ParticleEffectObject.ts" />
/// <reference path="../../particleEngine.ts" />

// The Three.js extensions we include.
// Note: They don't appear to be used.

module THREE {
    export var EffectComposer;
    export var RenderPass;
    export var ShaderPass;
    export var CopyShader;
}


module rmx.WebGL {

    export class Scene {

        scene    : THREE.Scene;
        renderer : THREE.WebGLRenderer;
        camera   : THREE.PerspectiveCamera;

        private ambientLight : THREE.AmbientLight;
        private sun          : THREE.DirectionalLight;

        private needsUpdateShadowCamera : boolean;

        particleEngine : ParticleEngine;

        objects   : RenderObject[];
        objectMap : { [id: string]: RenderObject };

        particleEffectObjects   : ParticleEffectObject[];
        particleEffectObjectMap : { [id: string]: ParticleEffectObject };

        pointerPosition : Vec2;
        // ^ The pointer position in client coordinates. It may be null if the
        // mouse is currently outside of the DOM element.

        lastUpdateAt: number; // DOMHighResTimeStamp
        // ^ The time when the canvas was last updated.

        generationNumber : number;
        // ^ A number which increments with each generation. Only render
        // objects which have a matching generation number are rendered. The
        // other objects are initially hidden, and eventually removed from the
        // scene.

        textureCache : TextureCache = new TextureCache;
        meshCache    : Loader;



        constructor() {
            this.onResizeEvent = this.onResizeEvent.bind(this);
            this.onMouseEvent  = this.onMouseEvent.bind(this);

            this.update        = this.update.bind(this);


            // Scene
            // -------------------------------------------------

            this.scene  = new THREE.Scene();

            this.particleEngine = new ParticleEngine(this.scene,
                    SoftwareParticleRenderer);

            // Camera
            // -------------------------------------------------

            this.camera = new THREE.PerspectiveCamera(75, 1, 1, 10000);
            this.camera.up.set(0, 0, 1);
            this.camera.position.set(10, 0, 10);
            this.camera.lookAt(new THREE.Vector3(0, 0, 0));

            this.scene.add(this.camera);


            // Renderer
            // -------------------------------------------------

            this.renderer = new THREE.WebGLRenderer({ antialias: true });
            this.renderer.setClearColor(new THREE.Color().setHex(0x888888));
            this.renderer.autoClear = true;

            // <canvas> doesn't seem to be a block element.
            this.renderer.domElement.style.display = 'block';

            // Globally disable context menu on the canvas.
            this.renderer.domElement.addEventListener('contextmenu', e => {
                e.preventDefault();
            });

            // var composer     = new THREE.EffectComposer(this.renderer);
            // var render_scene = new THREE.RenderPass(this.scene, this.camera);
            // render_scene.renderToScreen = true;

            // var effectScreen = new THREE.ShaderPass(THREE.CopyShader);
            // effectScreen.renderToScreen = true;

            // composer.addPass(render_scene);
            // composer.addPass(effectScreen);


            // Lights
            // -------------------------------------------------

            this.ambientLight = new THREE.AmbientLight(0x666666);
            this.scene.add(this.ambientLight);

            // Shadows
            // -------------------------------------------------

            this.needsUpdateShadowCamera = true;
            this.sun = new THREE.DirectionalLight(0xffffff);
            this.sun.position.set(70, 70, 100);

            this.sun.castShadow          = true;
            this.sun.shadowDarkness      = 0.6;    // Amount of blackness inside the shadow
            this.sun.shadowCameraVisible = false;  // Renders the shadow camera frustrum (for debugging)
            this.sun.shadowBias          = 0.0001; // Prevents z fighting
            // Manually optimized frustrum for the shadow camera
            this.sun.shadowCascade       = false;
            this.sun.shadowCameraFar     = 250;
            this.sun.shadowCameraNear    = 100;
            //this.sun.shadowCameraFov     = 15;
            this.sun.shadowCameraLeft    = -90;
            this.sun.shadowCameraRight   = 90;
            this.sun.shadowCameraTop     = 90;
            this.sun.shadowCameraBottom  = -90;

            //if options.get('enableHardShadows')
                //sun.shadowMapWidth = 1024;
                //sun.shadowMapHeight = 1024;
            //if options.get('enableSoftShadows')
            this.sun.shadowMapWidth = 2048;
            this.sun.shadowMapHeight = 2048;

            this.scene.add(this.sun);

            this.renderer.shadowMapEnabled  = true;
            this.renderer.shadowMapCascade  = false;
            this.renderer.shadowMapCullFace = THREE.CullFaceNone;
            this.renderer.shadowMapType     = THREE.PCFShadowMap;

            this.meshCache = new Loader(this);

            this.objects   = [];
            this.objectMap = Object.create(null);

            this.particleEffectObjects = [];
            this.particleEffectObjectMap = Object.create(null);

            this.pointerPosition  = null;
            this.lastUpdateAt     = null;
            this.generationNumber = 0;
        }

        setAmbientLight(light: string): void {
            var ambLight = parseInt(light, 16);
            if(!isNaN(ambLight))
                this.ambientLight.color = new THREE.Color(ambLight);
        }

        setSunLight(sunLight: string): void {
            var sLight = parseInt(sunLight, 16);
            if(sLight != NaN)
                this.sun.color = new THREE.Color(sLight);
        }

        setSunPos(pos: number[]): void {
            this.sun.position.set(pos[0], pos[1], pos[2]);
        }

        setShadowDarkness(value: number) {
            this.sun.shadowDarkness = value;
        }


        // domElement
        // -------------------------------------------------------------------
        //
        // The DOM element (a HTMLCanvasElement) into which the scene is being
        // rendered. You probably don't want to use this directly. Instead, use
        // 'appendTo' and 'removeFrom'.

        get domElement(): HTMLElement {
            return this.renderer.domElement;
        }


        // appendTo
        // -------------------------------------------------------------------
        //
        // Append the canvas to the given DOM node. This also starts the
        // render loop, which remains active until the canvas is removed from
        // the DOM (see 'removeFrom').

        appendTo(containerElement: HTMLElement): void {
            containerElement.appendChild(this.domElement);

            this.onResizeEvent();
            requestAnimationFrame(this.update);

            window.addEventListener('resize', this.onResizeEvent);

            this.domElement.addEventListener('mousedown',  this.onMouseEvent);
            this.domElement.addEventListener('mousemove',  this.onMouseEvent);
            this.domElement.addEventListener('mouseup',    this.onMouseEvent);
            this.domElement.addEventListener('mouseleave', this.onMouseEvent);
        }


        // removeFrom
        // -------------------------------------------------------------------
        //
        // Remove the canvas from the given DOM node. The node must be the
        // same as the one given to 'appendTo'. It is only used for sanity
        // checking.

        removeFrom(containerElement: HTMLElement): void {
            var parentElement = this.domElement.parentElement;

            window.removeEventListener('resize', this.onResizeEvent);

            this.domElement.removeEventListener('mousedown',  this.onMouseEvent);
            this.domElement.removeEventListener('mousemove',  this.onMouseEvent);
            this.domElement.removeEventListener('mouseup',    this.onMouseEvent);
            this.domElement.removeEventListener('mouseleave', this.onMouseEvent);

            if (containerElement !== parentElement) {
                console.warn('WebGL Scene: Container node has changed.');
            }

            if (parentElement) {
                parentElement.removeChild(this.domElement);
            }
        }

        onResizeEvent(): void {
            var parentElement = this.domElement.parentElement;
            if (parentElement) {
                requestAnimationFrame(() => {
                    var w  = parentElement.clientWidth
                      , h  = parentElement.clientHeight;

                    this.renderer.setSize(w, h);

                    this.camera.aspect = w / h;
                    this.camera.updateProjectionMatrix();
                });
            }
        }

        onMouseEvent(ev: MouseEvent): void {
            if (ev.type === 'mouseleave') {
                // Clear the pointer position to indicate that the mouse has
                // left the canvas.
                this.pointerPosition = null;

            } else {
                var x = offsetX(ev)
                  , y = offsetY(ev);

                // TODO: Consider using vec2.copy() if the pointer position
                // already exists, that will avoid one allocation each time
                // the mouse moves (which can potentially be a lot).
                this.pointerPosition = vec2.fromValues(x, y);
            }
        }



        // prepareRenderObject
        // -------------------------------------------------------------------
        //
        // Called at each update to prepare the RenderObject for rendering.
        // This function updates the underlying Three.js mesh correctly
        // represents the RenderObject.

        prepareRenderObject(now: number, dt: number, ro: RenderObject): void {
            syncModelMatrix(ro);
            syncVisual(this, ro);
            applyAnimation(ro, now, dt);
            updateVisualMatrix(ro);

            ro.root.visible = isCurrentGeneration(this, ro.generationNumber);
            ro.root.children.forEach(child => {
                child.visible = ro.root.visible;
            });
        }


        prepareParticleEffectObject(now: number, dt: number, ro:
                ParticleEffectObject): void {

            var engineHasEffect =
                this.particleEngine.particleEffects[ro.id];

            if (ro.particleEffect && !engineHasEffect) {
                //  ARGUMENTS: id, JSON, settings,
                //             [(string, Entity)], [(string, Model)]
                this.particleEngine.addParticleEffect(
                        ro.id, ro.particleEffect, ro.nem);
            }
        }


        // update
        // -------------------------------------------------------------------
        //
        // This function is meant to be used as the callback to rAF. It
        // assumes that the first argument is the current time
        // (DOMHighResTimeStamp).

        update(now: number): void {
            // The update loop automatically stops once the canvas element
            // is removed from the DOM.
            if (!this.domElement.parentElement) {
                // To unbind event handlers.
                this.removeFrom(null);

                return;
            }

            // Immediately schedule the next update.
            requestAnimationFrame(this.update);

            // Keep track of time.
            var dt = timeDelta(now, this.lastUpdateAt) / 1000;
            this.lastUpdateAt = now;

            // Prepare all RenderObjects for rendering.
            this.objects.forEach(this.prepareRenderObject.bind(
                this, now, dt));

            this.particleEffectObjects.forEach(
                this.prepareParticleEffectObject.bind(this, now, dt));

            this.particleEngine.update(now, dt);
            this.renderer.render(this.scene, this.camera);

            // Remove all invisible RenderObjects.
            this.objects.slice(0).forEach(obj => {
                if (!obj.root.visible) {
                    removeObject(this, obj);
                }
            });

            // Remove particle effects which are outdated.
            this.particleEffectObjects.slice(0).forEach(pe => {
                if (!pe.oneshot && !isCurrentGeneration(this, pe.generationNumber)) {
                    this.particleEngine.remove(pe.id);
                    this.particleEffectObjects = this.particleEffectObjects.filter(x => {
                        return x !== pe;
                    });
                } else if (pe.oneshot) {
                    if(this.particleEngine.hasFinished(pe.id)) {
                        this.particleEngine.remove(pe.id);
                        this.particleEffectObjects = this.particleEffectObjects.filter(x => {
                            return x !== pe;
                        });
                    }
                }
            });

            this.updateShadowCamera();
        }

        private updateShadowCamera(): void {

            if(!this.needsUpdateShadowCamera) return;
            if(!this.sun) return;

            var x0 = Infinity;
            var x1 = -Infinity;
            var y0 = Infinity;
            var y1 = -Infinity;
            var z0 = Infinity;
            var z1 = -Infinity;

            var pos  = new THREE.Vector3();
            var pmin = new THREE.Vector3();
            var pmax = new THREE.Vector3();

            var camera = new THREE.Camera();
            camera.position.copy(this.sun.position);
            camera.lookAt(this.sun.target.position);
            camera.updateMatrix();
            camera.updateMatrixWorld(false);
            var matrixWorldInverse = new THREE.Matrix4();
            matrixWorldInverse.getInverse(camera.matrixWorld);

            this.objects.forEach(tile => {

                if(!tile.visual) return;

                var geometry = (<THREE.Mesh>(tile.visual)).geometry;
                if(!geometry.boundingBox)
                    (<THREE.Mesh>(tile.visual)).geometry.computeBoundingBox();
                var bbox = geometry.boundingBox;

                var mat = new THREE.Matrix4();
                mat4.copy(mat.elements, tile.modelMatrix);
                pos.setFromMatrixPosition(mat);

                pmin.addVectors(pos, bbox.min);
                pmax.addVectors(pos, bbox.max);

                pmin.applyMatrix4(matrixWorldInverse);
                pmax.applyMatrix4(matrixWorldInverse);

                x0 = Math.min(x0, pmin.x);
                x1 = Math.max(x1, pmax.x);
                y0 = Math.min(y0, pmin.y);
                y1 = Math.max(y1, pmax.y);
                z0 = Math.min(z0, pmin.z);
                z1 = Math.max(z1, pmax.z);

            });

            var left   = x0 - 4;
            var right  = x1 + 4;
            var top    = y1 + 4;
            var bottom = y0 - 4;
            var near   = -z1 - 4;
            var far    = -z0 + 100;

            //if(this.sun.shadowCamera != null) {
               //this.sun.shadowCamera.left   = left;
               //this.sun.shadowCamera.right  = right;
               //this.sun.shadowCamera.top    = top;
               //this.sun.shadowCamera.bottom = bottom;
               //this.sun.shadowCamera.near   = near;
               //this.sun.shadowCamera.far    = far;
               //this.sun.shadowCamera.updateProjectionMatrix();
            //} else {
                this.sun.shadowCameraLeft   = left;
                this.sun.shadowCameraRight  = right;
                this.sun.shadowCameraTop    = top;
                this.sun.shadowCameraBottom = bottom;
                this.sun.shadowCameraFar    = far;
                this.sun.shadowCameraNear   = near;
            //}

            this.needsUpdateShadowCamera = false;
        }
    }


    // UserData
    // -----------------------------------------------------------------------
    //
    // Data store in a THREE.Object3D's userData property.

    export interface UserData {
        renderObject : RenderObject;
        // ^ The 'owner' of the object. Used for intersection and picking,
        // so we can map back from Three.js objects to our RenderObjects.

        visualKey    : any;
        // ^ A key representing the hash of the source which was used to
        // initialize the visual mesh. It is used to determine whether the
        // visual mesh needs to be updated or not.
    }


    export interface Intersection {
        object : THREE.Object3D;
        face   : THREE.Face3;
        point  : THREE.Vector3;

        owner  : RenderObject;
    }


    export class Loader {

        private cache : Map<rmx.Storage.Model, Mesh>;

        constructor
          ( public scene : Scene
          ) {
            this.cache = new Map<rmx.Storage.Model, Mesh>();
        }

        getMesh(model: rmx.Storage.Model): Mesh {
            var mesh = this.cache.get(model);

            if (!mesh) {
                mesh = createMesh(this.scene, model);
                this.cache.set(model, mesh);
            }

            return mesh;
        }
    }



    // intersectionAtPointer
    // ------------------------------------------------------------------
    //
    // If there is a Object underneath the current pointer position, return
    // some info about that object and the exact position in the world
    // where the ray intersects that object.

    export function
    intersectionAtPointer(scene: Scene, filter: (obj: THREE.Object3D) => boolean): Intersection {
        return intersectionsAtPointer(scene, filter)[0];
    }


    // intersectionsAtPointer
    // ------------------------------------------------------------------

    export function
    intersectionsAtPointer(scene: Scene, filter: (obj: THREE.Object3D) => boolean): Intersection[] {
        if (scene.pointerPosition) {
            var w = scene.domElement.clientWidth
              , h = scene.domElement.clientHeight
              , x = + (scene.pointerPosition[0] / w) * 2 - 1
              , y = - (scene.pointerPosition[1] / h) * 2 + 1;

            var raycaster = new THREE.Raycaster()
              , mouse     = new THREE.Vector3(x, y, 0.5);

            raycaster.setFromCamera(mouse, scene.camera);

            var objects    = scene.scene.children.filter(filter)
              , intersects = raycaster.intersectObjects(objects, true);

            return intersects.map(x => {
                return { object : x.object
                       , owner  : toRenderObject(x.object)
                       , face   : x.face
                       , point  : x.point
                       };
            });
        } else {
            return [];
        }
    }


    // timeDelta
    // -----------------------------------------------------------------------
    //
    // Return the time delta between an optional previous time and now. If
    // the previous time is null then this function returns null.

    function
    timeDelta(now: number, prev?: number): number {
        return prev === null ? 0 : now - prev;
    }


    // startNextGeneration
    // -----------------------------------------------------------------------
    //
    // Starts a new generation of the scene. Any RenderObjects become invalid
    // and need to be touched using 'renderObject'.

    export function
    startNextGeneration(scene: Scene): void {
        scene.generationNumber++;
    }


    // insertObject
    // -----------------------------------------------------------------------
    //
    // Insert a fresh RenderObject into the scene.

    function
    insertObject(scene: Scene, ro: RenderObject): void {
        scene.objects.push(ro);
        scene.objectMap[ro.id] = ro;

        scene.scene.add(ro.root);
    }


    // lookupRenderObject
    // -----------------------------------------------------------------------

    export function
    lookupRenderObject(scene: Scene, id: string): RenderObject {
        return scene.objectMap[id];
    }

    // renderObject
    // -----------------------------------------------------------------------
    //
    // Return a render object for the given Id. If one already exists in the
    // scene then it is reused, otherwise a new one is allocated. The render
    // object is guaranteed to have a matching generation number, so it will
    // be rendered in the next update.

    export function
    renderObject(scene: Scene, id: string): RenderObject {
        var ro = lookupRenderObject(scene, id);

        if (!ro) {
            ro = new RenderObject(id);
            insertObject(scene, ro);
        }

        if (ro.generationNumber === scene.generationNumber) {
            console.warn("WebGL renderObject: id '", id,
                "' was used twice in the same generation");
        }

        ro.generationNumber = scene.generationNumber;

        return ro;
    }


    // removeObject
    // -----------------------------------------------------------------------

    function
    removeObject(scene: Scene, ro: RenderObject): void {
        var index = scene.objects.indexOf(ro);
        if (index > -1) {
            scene.scene.remove(ro.root);
            //scene.particleEngine.remove(ro.id);

            scene.objects.splice(index, 1);
            delete scene.objectMap[ro.id];
        }
    }


    // isCurrentGeneration
    // -----------------------------------------------------------------------

    function
    isCurrentGeneration(scene: Scene, gen: number): boolean {
        return scene.generationNumber === gen;
    }


    // syncModelMatrix
    // -----------------------------------------------------------------------
    //
    // Update the 'modelMatrix' from the 'position' and 'rotation' properties.
    // (if they are set).

    function
    syncModelMatrix(ro: RenderObject): void {
        var pos = ro.position
          , rot = ro.rotation;

        if (pos && rot) {
            updateModelMatrix(ro, pos, rot[2], ro.rotationCenter, ro.scale);
        }
    }


    // syncVisual
    // -----------------------------------------------------------------------

    function
    syncVisual(scene: Scene, ro: RenderObject): void {
        if (ro.model === undefined) {
            // undefined means the user is manually managing the visual mesh,
            // so there is nothing to do here.

        } else {
            // The RenderObject has a model defined. We need to make sure it
            // matches the visual mesh.
            var visual = ro.visual;

            if (ro.model === null && visual) {
                // The user has explicitly set the model to null in order
                // to have the visual mesh removed from the scene.
                ro.root.remove(visual);

                var ud = <UserData> ro.root.userData;
                ud.visualKey = null;

            } else if (ro.model !== null) {
                // The RO model is set. Check if it matches the model used
                // to generate the visual.

                var visualKey = modelVisualKey(ro.model)
                  , mkMesh    = function() {
                        var mesh = scene.meshCache.getMesh(ro.model);
                        return createMeshInstance(mesh);
                    };

                updateVisual(scene, ro, visualKey, mkMesh);

            } else {
                // Model is null and no existing visual mesh. Nothing to do
                // here.
            }
        }
    }

    function modelVisualKey(model: rmx.Storage.Model): any {
        return model;
    }


    // toRenderObject
    // -----------------------------------------------------------------------
    //
    // Get the RenderObject associated with an THREE scene object.

    export function
    toRenderObject(obj: THREE.Object3D): RenderObject {
        return (<UserData>obj.userData).renderObject;
    }


    // updateVisualMatrix
    // -----------------------------------------------------------------------
    //
    // Copy the modelMatrix from the RenderObject into the root.

    function
    updateVisualMatrix(ro: RenderObject): void {
        // We use our own, precomputed matrix. Don't let threejs override it.
        ro.root.matrixAutoUpdate = false;
        ro.root.matrix.copy(<any> { elements: ro.modelMatrix });
    }


    // staticVisual
    // -----------------------------------------------------------------------

    export function
    staticVisual(scene: Scene, ro: RenderObject, mkVisual: () => THREE.Object3D): void {
        updateVisual(scene, ro, 'static', mkVisual);
    }


    // Return true if the visual key matches the one of the THREE.Object3D.
    function eqVisualKey(visual: THREE.Object3D, visualKey: string): boolean {
        if (!visual) {
            return false;
        } else {
            var userData = <UserData> visual.userData;
            return visualKey === userData.visualKey;
        }
    }


    // updateVisual
    // -----------------------------------------------------------------------
    //
    // Update the visible mesh of a 'RenderObject'. The visualKey should
    // represent the hash of the source and should change whenever the source
    // changes. The 'mkObj' function will be only used when they keys don't
    // match. So it can be quite expensive.

    export function
    updateVisual
    ( scene     : Scene
    , ro        : RenderObject
    , visualKey : any
    , mkObj     : () => THREE.Object3D
    ): void {
        if (eqVisualKey(ro.root, visualKey)) {
            // The visual keys match. Nothing to do.
            return;
        }

        var obj = mkObj();
        if (!obj) {
            // Hm, object could not be created. Doens't matter, we'll try
            // it again in the next frame. Hopefully it'll be loaded by then.
            return;
        }

        obj.receiveShadow = true;
        obj.castShadow    = ro.castShadow;

        // Remove the old visual. It's safe if 'ro.visual' is
        // undefined, Three.js should be able to handle that.
        ro.root.remove(ro.visual);

        obj.name = 'visual';
        ro.root.add(obj);

        // Reset animation state.
        ro.animationState = null;

        var ud = <UserData> ro.root.userData;
        ud.visualKey = visualKey;


        // Set the 'renderObject' on the whole object hierarchy. The
        // Scene needs to know the owner of every mesh when it does
        // intersection and picking.

        ro.root.traverse(x => {
            (<UserData>x.userData).renderObject = ro;
        });
    }


    function insertParticleEffectObject(
        scene: Scene, rpo: ParticleEffectObject): void {

        scene.particleEffectObjects.push(rpo);
        scene.particleEffectObjectMap[rpo.id] = rpo;
    }


    function lookupParticleEffectObject(
        scene: Scene, id: string): ParticleEffectObject {

        return scene.particleEffectObjectMap[id];
    }


    export function particleEffectObject(
        scene: Scene, json: rmx.Storage.ParticleEffect,
        nem: NameEntityMap,
        id: string, oneshot: boolean): ParticleEffectObject {

        var ro = lookupParticleEffectObject(scene, id);

        if (!ro) {
            ro = new ParticleEffectObject(id, json, nem);
            insertParticleEffectObject(scene, ro);
        }

        if (ro.generationNumber === scene.generationNumber) {
            console.warn("WebGL particleEffectObject: id '", id,
                "' was used twice in the same generation");
        }

        ro.generationNumber = scene.generationNumber;
        ro.oneshot = oneshot;

        return ro;
    }


    // Helper functions
    // -----------------------------------------------------------------------

    export function
    offsetX(ev: any): number {
        return ev.clientX - ev.target.getBoundingClientRect().left;
    }

    export function
    offsetY(ev: any): number {
        return ev.clientY - ev.target.getBoundingClientRect().top;
    }

    export function
    eventX(ev: any): number {
        return (offsetX(ev) / ev.target.clientWidth) * 2 - 1;
    }

    export function
    eventY(ev: any): number {
        return -(offsetY(ev) / ev.target.clientHeight) * 2 + 1;
    }
}
