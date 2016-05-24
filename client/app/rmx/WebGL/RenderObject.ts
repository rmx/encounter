/// <reference path="../../ext/three.d.ts" />
/// <reference path="../../ext/gl-matrix.ts" />

/// <reference path="../Storage/Types.ts" />
/// <reference path="./Animation.ts" />


module rmx.WebGL {

    export class RenderObject {

        // Primary properties
        // -------------------------------------------------------------------
        //
        // These must be set at all times.

        generationNumber : number = null;
        // ^ Used to determine whether the RenderObject is visible and should
        // be rendered into the canvas.

        root : THREE.Object3D = new THREE.Object3D();
        // ^ The THREE object representing the RO. It is always valid, the
        // actual visible mesh is added as a child to it. The modelMatrix
        // is copied into this object. All its children should have position
        // and rotation untouched.

        modelMatrix : Mat4 = mat4.create();
        // ^ The world matrix of the RO. During each update, this matrix is
        // copied into the root's modelMatrix.
        //
        // You can either set it directly or let it be automatically computed
        // from 'position' and 'rotation'.



        // Secondary properties
        // -------------------------------------------------------------------
        //
        // The secondary properties are used to compute or update the primary
        // properties. They are all optional.

        model : rmx.Storage.Model = undefined;
        // ^ If set, then the visible mesh will be generated from this model.
        // Can be changed at any time, the main loop will regenerate the
        // visible mesh if it doesn't match this model.

        skinId : string = undefined;
        // ^ An optional skinId, in case the 'model' has more than one skin.

        animation : Animation = undefined;
        // ^ The abstract description of the animation which should be active.
        // Once the THREE mesh is loaded, this is used together with a valid
        // 'model' as input to compute the 'animationState', which then drives
        // the keyframe animations.

        animationState : AnimationState = undefined;
        // ^ Stores the mesh-dependent animation state, and is used to update
        // the mesh bones before each frame.


        position : Vec3 = undefined;
        // ^ If set, then the 'modelMatrix' will be generated from this
        // position. You probably also want to set 'rotation'.

        rotation : Vec3 = undefined;
        // ^ Optional, used to compute the 'modelMatrix'. Only the Z-axis
        // is used.

        rotationCenter : Vec3 = undefined;
        // ^ Optional rotation center around which to apply the 'rotation'.

        scale : Vec3 = undefined;
        // ^ Optional scale


        //particleEffect   : rmx.Storage.ParticleEffect;
        // ^ TODO: Particle effects should be stored separately, not as ROs.


        selectable : boolean = true;
        // ^ FIXME: It's wrong to assume the whole RO can be selectable or
        // not. Maybe only some parts are (hitbox) and others aren't (visual
        // mesh). It should be up to the caller to determine which objects
        // he wants the ray to intersect with.


        castShadow : boolean = true;
        // ^ Not all objects cast a shadow. By default they do but this can be
        // disabled.


        constructor(public id: string) {
            this.root.receiveShadow = true;
            mat4.identity(this.modelMatrix);
        }


        // visual
        // -------------------------------------------------------------------
        //
        // The visible part of the object. There may only be at most one. If
        // not set, then nothing will be visible in the scene. Use
        // 'updateVisual' to change it.

        get visual(): THREE.Object3D {
            return this.root.getObjectByName('visual', false);
        }


        //FIXME: we should us the proper THREE.BoundingBoxHelper to get
        //       center, size, radius
        getMeshCenter(): THREE.Vector3 {
            if (this.visual) {
                // FIXME: What if the mesh is not THREE.Mesh, such as THREE.Line?
                var geometry = (<THREE.Mesh>this.visual).geometry;
                geometry.computeBoundingBox();

                var min = geometry.boundingBox.min;
                var max = geometry.boundingBox.max;

                var ret = new THREE.Vector3(0, 0, 0);
                ret.addVectors(min, max);
                ret.multiplyScalar(0.5);
                ret.applyMatrix4(this.root.matrixWorld);
                return ret;
            } else {
                return new THREE.Vector3(0, 0, 0);
            }
        }


        getBoundingBoxHeight(): number {
            if (this.visual) {
                // FIXME: What if the mesh is not THREE.Mesh, such as THREE.Line?
                var geometry = (<THREE.Mesh>this.visual).geometry;
                geometry.computeBoundingBox();

                var min = geometry.boundingBox.min;
                var max = geometry.boundingBox.max;

                max.sub(min);
                return max.z;
            } else {
                return 0;
            }
        }


        getBoundingSphere(): THREE.BoundingSphere {
            if (this.visual) {
                // FIXME: What if the mesh is not THREE.Mesh, such as THREE.Line?
                var geometry = (<THREE.Mesh>this.visual).geometry;
                geometry.computeBoundingSphere();
                return geometry.boundingSphere;
            } else {
                return null;
            }
        }
    }



    // Animation
    // ------------------------------------------------------------------
    //
    // This class stores animation data that is not specific to the mesh
    // used by the Object.
    //
    // TODO: Allow setting/overriding loop behavior, duration, ...

    export class Animation {
        constructor
          ( public name      : string
          , public startedAt : number //FIXME: use elapsed (time)
          , public loop      : boolean
          , public frozen    : boolean
          , public lastAnimationFrame : number
          ) {}
    }



    // updateModelMatrix
    // -----------------------------------------------------------------------

    export function
    updateModelMatrix
    ( ro         : RenderObject
    , pos        : Vec3
    , rotZ       : number
    , rotCenter ?: Vec3
    , scale     ?: Vec3
    ): void {
        var center, mat = ro.modelMatrix;
        mat4.identity(mat);

        mat4.translate(mat, mat, pos);

        if (rotCenter) {
            mat4.translate(mat, mat, rotCenter);
            center = vec3.clone(rotCenter);
        }

        mat4.rotateZ(mat, mat, rotZ);

        if (rotCenter) {
            vec3.scale(center, center, -1);
            mat4.translate(mat, mat, center);
        }

        if (scale) {
            mat4.scale(mat, mat, scale);
        }
    }
}
