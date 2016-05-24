/// <reference path="../../ext/gl-matrix.ts" />

/// <reference path="../Game/Client.ts" />
/// <reference path="../WebGL/Scene.ts" />
/// <reference path="../WebGL/Utils.ts" />

module rmx.Editor {

    import toVec3      = rmx.WebGL.toVec3;

    import KeyState    = rmx.Core.KeyState;
    import Input       = rmx.Core.Input;
    import Bindings    = rmx.Core.Bindings;
    import InputSource = rmx.Core.InputSource;


    export function
    applyCameraTo
    ( cam     : Game.Camera
    , camera  : THREE.PerspectiveCamera
    , target  : Vec3
    , height  : number = 0
    ): void {

        var pos = cam.positionRelativeTo(target);
        camera.position.set(pos[0], pos[1], pos[2]);

        // zoom to model boundary box (if available)
        var targetV3 = new THREE.Vector3(target[0], target[1], target[2]);
        var distance = vec3.distance(pos, target);
        if(height > 0) {
            camera.fov = 2.0 * Math.atan(1.4 * height / 2.0 / distance)
                         * (180 / Math.PI);
            camera.updateProjectionMatrix();
        }

        camera.lookAt(targetV3);
    }




    export module ModelRendererActions {

        ModelRendererActions["camera-zoom-in"] =
        (ctx : ModelRenderer, keyState: KeyState) => {
            ctx.camera.changeZoom(+1);
        };

        ModelRendererActions["camera-zoom-out"] =
        (ctx : ModelRenderer, keyState: KeyState) => {
            ctx.camera.changeZoom(-1);
        };

        ModelRendererActions["camera-move"] =
        (ctx : ModelRenderer, keyState: KeyState) => {
            var movement = ctx.input.pointerMovement(1);
            if (movement && movement[0] !== 0)  {
                var headingDiff = 2 * Math.PI * movement[0]
                  , pitchDiff   = Math.PI * movement[1];

                ctx.camera.changeHeading(headingDiff);
                ctx.camera.changePitch(pitchDiff);
            }
        };

    }

    export class ModelRenderer {

        input         : Input;
        bindings      : Bindings<ModelRenderer>;
        camera        : Game.Camera;

        model         : rmx.Storage.Model;
        animationName : string;

        scene         : WebGL.Scene;
        inputSource   : InputSource;


        constructor() {
            this.input        = new Input;
            this.bindings     = new Bindings<ModelRenderer>(<any>ModelRendererActions);
            this.camera       = new Game.Camera;

            this.scene        = new WebGL.Scene;
            this.inputSource  = new InputSource(this.input, this.bindings, this.scene.domElement);

            this.scene.renderer.setClearColor(new THREE.Color().setHex(0xFFFFFF));

            // Default bindings
            this.bindings.bindings['button-1']    = { action: 'camera-move'     };
        }


        update(now: number): void {
            rmx.Core.dispatchInput(now, this.input, this.bindings, this);

            WebGL.startNextGeneration(this.scene);

            var ro = WebGL.renderObject(this.scene, 'ro');

            var height       = ro.getBoundingBoxHeight();
            var cameraTarget = ro.getMeshCenter();

            applyCameraTo(this.camera, this.scene.camera,
                          toVec3(cameraTarget), height);

            ro.model = this.model;
            if (this.animationName && (!ro.animation || ro.animation.name !== this.animationName)) {
                ro.animation = new rmx.WebGL.Animation
                    ( this.animationName
                    , now
                    , true
                    , false
                    , 0
                    );
            }
        }
    }
}
