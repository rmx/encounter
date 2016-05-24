module rmx {

    import KeyState    = rmx.Core.KeyState;
    import Input       = rmx.Core.Input;
    import Bindings    = rmx.Core.Bindings;
    import InputSource = rmx.Core.InputSource;


    export module ParticleEffectRendererActions {

        ParticleEffectRendererActions["camera-zoom-in"] =
        (ctx : ParticleEffectRenderer, keyState: KeyState) => {
            ctx.camera.changeZoom(+1);
        };

        ParticleEffectRendererActions["camera-zoom-out"] =
        (ctx : ParticleEffectRenderer, keyState: KeyState) => {
            ctx.camera.changeZoom(-1);
        };

        ParticleEffectRendererActions["camera-move"] =
        (ctx : ParticleEffectRenderer, keyState: KeyState) => {
            var movement = ctx.input.pointerMovement(1);
            if (movement && movement[0] !== 0)  {
                var headingDiff = 2 * Math.PI * movement[0]
                  , pitchDiff   = Math.PI * movement[1];

                ctx.camera.changeHeading(headingDiff);
                ctx.camera.changePitch(pitchDiff);
            }
        };
    }

    export class ParticleEffectRenderer {

        input         : Input;
        bindings      : Bindings<ParticleEffectRenderer>;
        camera        : Game.Camera;

        scene         : WebGL.Scene;
        inputSource   : InputSource;

        particleEffect : rmx.Storage.ParticleEffect;

        // stubbing entity
        private target : any;


        constructor() {
            this.input        = new Input;
            this.bindings     = new Bindings<ParticleEffectRenderer>(
                                    <any>ParticleEffectRendererActions);
            this.camera       = new Game.Camera;

            this.scene        = new WebGL.Scene;
            this.inputSource  = new InputSource(this.input, this.bindings,
                                                this.scene.domElement);

            // Default bindings
            this.bindings.bindings['wheelup']   =
                { action: 'camera-zoom-in'  };
            this.bindings.bindings['wheeldown'] =
                { action: 'camera-zoom-out' };
            this.bindings.bindings['button-1']  =
                { action: 'camera-move'     };

            this.target = {
                position: vec3.fromValues(0.0, 0.0, 0.0)
            };
        }


        update(now: number): void {
            rmx.Core.dispatchInput(now, this.input, this.bindings, this);

            WebGL.startNextGeneration(this.scene);

            this.target.position = vec3.fromValues(
                6.0 * Math.cos(now * 2 * Math.PI / 5000.0),
                6.0 * Math.sin(now * 2 * Math.PI / 5000.0), 0.0);

            var movingTarget = WebGL.renderObject(this.scene, 'target');
            movingTarget.position = this.target.position;
            movingTarget.rotation = vec3.fromValues(0.0, 0.0, 0.0);
            WebGL.staticVisual(this.scene, movingTarget, function() {
                var geometry = new THREE.BoxGeometry(1, 1, 1);
                var material = new THREE.MeshBasicMaterial(
                                    { color: 0xe3e3e3 });
                return new THREE.Mesh(geometry, material);
            });

            var selfTarget = WebGL.renderObject(this.scene, 'self');
            selfTarget.position = vec3.fromValues(0.0, 0.0, 0.0);
            selfTarget.rotation = vec3.fromValues(0.0, 0.0, 0.0);
            WebGL.staticVisual(this.scene, selfTarget, function() {
                var geometry = new THREE.BoxGeometry(1, 1, 1);
                var material = new THREE.MeshBasicMaterial(
                                    { color: 0xe1e1e1 });
                return new THREE.Mesh(geometry, material);
            });

            // stubbing id -> entity mapping for pe
            var nem: rmx.WebGL.NameEntityMap =
                { self   : selfTarget
                , target : movingTarget
                };

            WebGL.particleEffectObject(
                this.scene, this.particleEffect, nem, 'particleEffect',
                true);

            // applyCameraTo takes a Vec3 as the camera target.
            rmx.Editor.applyCameraTo(this.camera, this.scene.camera, <any>[0,0,0]);
        }

        startEffect(): void {
            var nem: rmx.WebGL.NameEntityMap =
                { self   : rmx.WebGL.lookupRenderObject(this.scene, "self")
                , target : rmx.WebGL.lookupRenderObject(this.scene, "target")
                };

            WebGL.particleEffectObject
                ( this.scene
                , this.particleEffect
                , nem
                , 'particleEffect' + Math.random()
                , true
                );
        }
    }
}
