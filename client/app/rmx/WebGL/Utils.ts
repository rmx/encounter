/// <reference path="../../ext/three.d.ts" />
/// <reference path="../../ext/gl-matrix.ts" />

module THREE { export var ColorConverter: any; }

module rmx.WebGL {

    export function toThreeVec3(vec: Vec3): THREE.Vector3 {
        var ret = THREEVectorPool.get().set(vec[0], vec[1], vec[2]);
        VectorPool.release(vec);
        return ret;
    }

    export function toVec3(vec: THREE.Vector3): Vec3 {
        var ret = VectorPool.get();
        ret[0] = vec.x; ret[1] = vec.y; ret[2] = vec.z;
        THREEVectorPool.release(vec);
        return ret;
    }

    export function setHSV(color: number[],
                           h: number, s: number, v: number): any {
        return THREE.ColorConverter.setHSV(color, h, s, v);
    }

}

module rmx.WebGL.THREEVectorPool {

    var pools       : THREE.Vector3[] = [];
    var maxPoolSize : number          = 1000;

    export function get(): THREE.Vector3 {
        if (pools.length > 0)
            return pools.pop();

        return addToPool();
    }

    export function release(vector: THREE.Vector3): void {
        pools.push(vector);
    }

    function addToPool(): THREE.Vector3 {
        for (var i = 0; i < maxPoolSize; ++i) {
            pools.push(new THREE.Vector3());
        }
        return new THREE.Vector3();
    }
}

module rmx.WebGL.VectorPool {

    var pools       : Vec3[] = [];
    var maxPoolSize : number = 1000;

    export function get(): Vec3 {
        if (pools.length > 0)
            return pools.pop();

        return addToPool();
    }

    export function release(vector: Vec3): void {
        pools.push(vector);
    }

    function addToPool(): Vec3 {
        for (var i = 0; i < maxPoolSize; ++i) {
            pools.push(vec3.create());
        }
        return vec3.create();
    }
}

module rmx.WebGL.ParticleEffectUtils {

    export var DEGREE_TO_RADIAN : number = Math.PI / 180;
    export var TWOPI            : number = Math.PI * 2;

    export function uniformRandomInDisc(minr: number, maxr: number): number[] {
        var minr_scaled = minr / maxr;
        var t = this.TWOPI * Math.random();
        var u = Math.random() + Math.random();
        var r = u;
        if (u > 1.0) {
            r = 2.0 - u;
        }
        if (r < minr_scaled) {
            r = minr_scaled + r * ((1.0 - minr_scaled) / minr_scaled);
        }
        return [maxr * r * Math.cos(t), maxr * r * Math.sin(t)];
    }

    export function getPerpendiculars(normal: Vec3): Vec3[] {
        var random = vec3.fromValues(1, 0, 0);
        var axis1  = vec3.fromValues(0, 0, 0);
        var axis2  = vec3.fromValues(0, 0, 0);

        vec3.cross(axis1, normal, random);
        vec3.cross(axis2, axis1, normal);
        vec3.normalize(axis1, axis1);
        vec3.normalize(axis2, axis2);
        return [axis1, axis2];
    }
}
