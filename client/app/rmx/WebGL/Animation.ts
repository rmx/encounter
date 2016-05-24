/// <reference path="./RenderObject.ts" />

module rmx.WebGL {

    // applyAnimation
    // -----------------------------------------------------------------------
    //
    // This function updates the meshes with the selected animation. The
    // RenderObject must have both `animation` and `model` components
    // attached.

    export function
    applyAnimation
    ( ro  : WebGL.RenderObject
    , now : number
    , dt  : number
    ): void {
        var animation = ro.animation
          , visual    = ro.visual;

        if (visual && animation) {
            var animationLabel = findAnimationLabel(ro.model, animation.name);
            if (animationLabel) {
                if (!ro.animationState) {
                    ro.animationState = new AnimationState(<any>visual);
                }

                var currentKeyframe = 0;

                if(animation.frozen) {
                    animation.startedAt += dt;
                    currentKeyframe = ro.animation.lastAnimationFrame;
                } else
                    currentKeyframe = computeCurrentKeyframe(animation, animationLabel, now);
                ro.animation.lastAnimationFrame = currentKeyframe;
                ro.animationState.applyKeyframe(currentKeyframe);
            }
        }
    }


    export function
    findAnimationLabel
    ( model : rmx.Storage.Model
    , name  : string
    ): rmx.Storage.AnimationLabel {
        return model.animationLabels.filter(x => { return x.name == name; })[0];
    }

    function
    computeCurrentKeyframe
    ( animation      : any
    , animationLabel : rmx.Storage.AnimationLabel
    , now            : number
    ): number {
        var fps      = animationLabel.fps;
        var duration = (animationLabel.end - animationLabel.start) / fps
          , offset   = (now - animation.startedAt) / 1000;

        if (offset < duration || animation.loop) {
            return animationLabel.start + (offset % duration) * fps;
        } else {
            return animationLabel.end;
        }
    }



    // AnimationState
    // -----------------------------------------------------------------------
    //
    // The animation state stores mesh-specific state which is required to
    // apply keyframes to the mesh. The state is created as necessary, when
    // the entity has an animation but no state yet.

    export interface PRQS {
        p : THREE.Vector3;
        r : THREE.Euler;
        q : THREE.Quaternion;
        s : THREE.Vector3;
    }

    export interface AnimationData {
        hierarchy   : any;
        hasTangents : boolean;
    }

    export class AnimationState {

        restPose      : PRQS[];
        animationData : AnimationData;

        constructor
          ( public mesh : THREE.SkinnedMesh
          ) {
            this.restPose = (<any>mesh).skeleton.bones.map(bone => {
                return { p: bone.position.clone()
                       , r: bone.rotation.clone()
                       , q: bone.quaternion.clone()
                       , s: bone.scale.clone()
                       };
            });


            // r68 doesn't have 'animation' property on a Geometry object.
            // But the JSON loader sets it anyway when it's a SkinnedMesh.
            this.animationData = (<any>mesh.geometry).animation;
            if (this.animationData && !this.animationData.hasTangents) {
                for (var boneIndex in this.animationData.hierarchy) {
                    var bone = this.animationData.hierarchy[boneIndex];

                    for (var keyIndex in bone.keys) {
                        var key = bone.keys[keyIndex];

                        // Compute tangents for all keys with position data
                        if (key.pos) {
                            key.index = keyIndex;
                            var limits = binaryRangeFinder(bone.keys, "pos", key.time);
                            var key0 = getKeyWithSmallerTime(bone.keys, "pos", key.time, limits);
                            var key1 = key;
                            var key2 = getKeyWithGreaterTime(bone.keys, "pos", key.time, limits);

                            // http://en.wikipedia.org/wiki/Cubic_Hermite_spline#Finite_difference
                            key.dpos = [0,0,0];

                            // XXX: tslint: duplicate variable
                            var d;

                            if (key0) {
                                d = 2 * (key1.time - key0.time);
                                key.dpos[0] += (key1.pos[0] - key0.pos[0]) / d;
                                key.dpos[1] += (key1.pos[1] - key0.pos[1]) / d;
                                key.dpos[2] += (key1.pos[2] - key0.pos[2]) / d;
                            }

                            if (key2) {
                                d = 2 * (key2.time - key1.time);
                                key.dpos[0] += (key2.pos[0] - key1.pos[0]) / d;
                                key.dpos[1] += (key2.pos[1] - key1.pos[1]) / d;
                                key.dpos[2] += (key2.pos[2] - key1.pos[2]) / d;
                            }
                        }
                    }
                }

                this.animationData.hasTangents = true;
            }
        }

        applyKeyframe(currentFrame: number): void {

            var absoluteTime = currentFrame;

            // FIXME: Three.js r68
            var _ref = (<any>this.mesh).skeleton.bones;

            for (var boneIndex = 0; boneIndex < _ref.length; boneIndex++) {

                var bone = _ref[boneIndex];
                var boneKeys = this.animationData.hierarchy[boneIndex].keys;

                var _ref1 = getKeysWith(boneKeys, "pos", absoluteTime);
                var p0 = _ref1[0];
                var p1 = _ref1[1];
                var t  = _ref1[2];
                if (t != null) {
                    interpolateCubic(bone.position, p0.pos, p1.pos,
                                     p0.dpos, p1.dpos, t);
                }

                var _ref2 = getKeysWith(boneKeys, "rot", absoluteTime);
                if (_ref2[2] != null) {
                    interpolateSLerp(bone.quaternion,
                                     _ref2[0].rot, _ref2[1].rot, _ref2[2]);
                }

                var _ref3 = getKeysWith(boneKeys, "scl", absoluteTime);
                if (_ref3[2] != null) {
                    interpolateLinear(bone.scale,
                                      _ref3[0].scl, _ref3[1].scl, _ref3[2]);
                }

                bone.updateMatrix();
                bone.matrixWorldNeedsUpdate = true;
            }
        }

        reset() {
            var bone, boneIndex, restPose, _i, _len, _ref;

            // FIXME: Three.js r68
            _ref = (<any>this.mesh).skeleton.bones;

            for (boneIndex = _i = 0, _len = _ref.length; _i < _len; boneIndex = ++_i) {
                bone = _ref[boneIndex];
                restPose = this.restPose[boneIndex];
                bone.position.copy(restPose.p);
                bone.rotation.copy(restPose.r);
                bone.quaternion.copy(restPose.q);
                bone.scale.copy(restPose.s);
            }
        }
    }


    function getKeysWith(keys, type, time) {
        var limits = binaryRangeFinder(keys, type, time);
        var k0 = getKeyWithSmallerEqualTime(keys, type, time, limits);
        var k1 = getKeyWithGreaterTime(keys, type, time, limits);
        if ((k0 != null) && (k1 != null)) {
            var t = (time - k0.time) / (k1.time - k0.time);
            return [k0, k1, t];
        } else {
            return [null, null, null];
        }
    }

    function getKeyWithSmallerEqualTime(keys, type, time, limits) {

        //FIXME: for loop can be removed as well
        for (var i = limits[1]; i >= limits[0]; i--) {
            var key = keys[i];
            if (key.time <= time && (key[type] != null)) {
                return key;
            }
        }
    }

    function getKeyWithSmallerTime(keys, type, time, limits) {

        //FIXME: for loop can be removed as well
        for (var i = limits[1]; i >= limits[0]; i--) {
            var key = keys[i];
            if (key.time < time && (key[type] != null)) {
                return key;
            }
        }
    }

    function getKeyWithGreaterTime(keys, type, time, limits) {

        //FIXME: for loop can be removed as well
        for (var i = limits[0]; i <= limits[1]; i++) {
            var key = keys[i];
            if (key.time > time && (key[type] != null)) {
                return key;
            }
        }
    }

    function binaryRangeFinder(keys, type, time) {

        var minIndex = 0;
        var maxIndex = keys.length - 1;
        var lastSmallerIndex = minIndex;
        var lastGreaterIndex = maxIndex;

        var curIndex;
        var curTime;

        while (minIndex < maxIndex) {
            curIndex = (minIndex + maxIndex) / 2 | 0;
            curTime  = keys[curIndex].time;

            if (curTime < time) {
                lastSmallerIndex = minIndex;
                minIndex = curIndex + 1;
            } else if (curTime > time) {
                lastGreaterIndex = maxIndex;
                maxIndex = curIndex - 1;
            } else {
                break;
            }
        }

        return [lastSmallerIndex, lastGreaterIndex];
    }

    function interpolateLinear(result: THREE.Vector3, x0, x1, t) {
        var h0 = 1 - t
          , h1 = t;

        result.x = x0[0] * h0 + x1[0] * h1;
        result.y = x0[1] * h0 + x1[1] * h1;
        result.z = x0[2] * h0 + x1[2] * h1;
    }

    function interpolateCubic(result: THREE.Vector3, x0, x1, d0, d1, t) {
        var t2  = t * t
          , t3  = t * t2
          , h00 = 2 * t3 - 3 * t2 + 1
          , h10 = t3 - 2 * t2 + t
          , h01 = -2 * t3 + 3 * t2
          , h11 = t3 - t2;

        result.x = h00 * x0[0] + h10 * d0[0] + h01 * x1[0] + h11 * d1[0];
        result.y = h00 * x0[1] + h10 * d0[1] + h01 * x1[1] + h11 * d1[1];
        result.z = h00 * x0[2] + h10 * d0[2] + h01 * x1[2] + h11 * d1[2];
    }

    // FIXME: result is THREE.Quaternion but there are no _{x,y,z,w} properties
    // on that.
    function interpolateSLerp(result, x0, x1, t) {
        var cosHalfTheta = x0[0] * x1[0] + x0[1] * x1[1] + x0[2] * x1[2] + x0[3] * x1[3];
        if (cosHalfTheta < 0) {
            x1 = [-x1[0], -x1[1], -x1[2], -x1[3]];
            cosHalfTheta = -cosHalfTheta;
        }

        if (cosHalfTheta >= 1.0) {
            result._x = x0[0];
            result._y = x0[1];
            result._z = x0[2];
            result._w = x0[3];
        } else {
            var halfTheta    = Math.acos(cosHalfTheta)
              , sinHalfTheta = Math.sqrt(1.0 - cosHalfTheta * cosHalfTheta);

            if (Math.abs(sinHalfTheta) < 0.001) {
                result._x = 0.5 * (x0[0] + x1[0]);
                result._y = 0.5 * (x0[1] + x1[1]);
                result._z = 0.5 * (x0[2] + x1[2]);
                result._w = 0.5 * (x0[3] + x1[3]);
            } else {
                var ratioA = Math.sin((1 - t) * halfTheta) / sinHalfTheta
                  , ratioB = Math.sin(t * halfTheta) / sinHalfTheta;

                result._x = x0[0] * ratioA + x1[0] * ratioB;
                result._y = x0[1] * ratioA + x1[1] * ratioB;
                result._z = x0[2] * ratioA + x1[2] * ratioB;
                result._w = x0[3] * ratioA + x1[3] * ratioB;
            }
        }
    }

}
