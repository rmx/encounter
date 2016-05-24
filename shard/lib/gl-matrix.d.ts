// glMatrix 2.0
// -----------------------------------------------------------------------------

declare module "gl-matrix" {
    export module glMatrix {
        export function setMatrixArrayType(type: any): void;
    }


    // mat2
    // -----------------------------------------------------------------------------

    export type Mat2 = Float32Array | number[];
    export module mat2 {
        export function adjoint(out, a)
        export function clone(a);
        export function copy(out, a)
        export function create()
        export function determinant(a)
        export function identity(out)
        export function invert(out, a)
        export function mul()
        export function multiply(out, a, b)
        export function rotate(out, a, rad)
        export function scale(out, a, v)
        export function str(mat)
        export function transpose(out, a)
    }



    // mat2d
    // -----------------------------------------------------------------------------

    export type Mat2d = Float32Array | number[];
    export module mat2d {
        export function clone(a)
        export function copy(out, a)
        export function create()
        export function determinant(a)
        export function identity(out)
        export function invert(out, a)
        export function mul()
        export function multiply(out, a, b)
        export function rotate(out, a, rad)
        export function scale(out, a, v)
        export function str(a)
        export function translate(out, a, v)
    }



    // mat3
    // -----------------------------------------------------------------------------

    export type Mat3 = Float32Array | number[];
    export module mat3 {
        export function adjoint(out: Mat3, a: Mat3): Mat3;
        export function clone(a: Mat3): Mat3;
        export function copy(out: Mat3, a: Mat3): Mat3;
        export function create(): Mat3;
        export function determinant(a: Mat3): number;
        export function fromMat2d(out: Mat3, a: Mat2d): Mat3;
        export function fromMat4(out: Mat3, a: Mat4): Mat3;
        export function fromQuat(out: Mat3, q: Quat): Mat3;
        export function identity(out: Mat3): Mat3;
        export function invert(out: Mat3, a: Mat3): Mat3;
        export function mul(out: Mat3, a: Mat3, b: Mat3): Mat3;
        export function multiply(out: Mat3, a: Mat3, b: Mat3): Mat3;
        export function normalFromMat4(out: Mat3, a: Mat4): Mat3;
        export function rotate(out: Mat3, a: Mat3, rad: number): Mat3;
        export function scale(out: Mat3, a: Mat3, v: Vec2): Mat3;
        export function str(mat: Mat3): string;
        export function translate(out: Mat3, a: Mat3, v: Vec2): Mat3;
        export function transpose(out: Mat3, a: Mat3): Mat3;
    }



    // mat4
    // -----------------------------------------------------------------------------

    export type Mat4 = Float32Array | number[];
    export module mat4 {
        export function create(): Mat4;
        export function clone(a: Mat4): Mat4;
        export function adjoint(out: Mat4, a: Mat4): Mat4;
        export function copy(out: Mat4, a: Mat4): Mat4;
        export function determinant(a: Mat4): number;
        export function fromQuat(out: Mat4, q: Quat): Mat4;
        export function fromRotationTranslation(out: Mat4, q: Quat, v: Vec3): Mat4;
        export function frustum(out: Mat4, left: number, right: number, bottom: number, top: number, near: number, far: number): Mat4;
        export function identity(out: Mat4): Mat4;
        export function invert(out: Mat4, a: Mat4): Mat4;
        export function lookAt(out: Mat4, eye: Vec3, center: Vec3, up: Vec3): Mat4;
        export function mul(out: Mat4, a: Mat4, b: Mat4): Mat4;
        export function multiply(out: Mat4, a: Mat4, b: Mat4): Mat4;
        export function ortho(out: Mat4, left: number, right: number, bottom: number, top: number, near: number, far: number): Mat4;
        export function perspective(out: Mat4, fovy: number, aspect: number, near: number, far: number): Mat4;
        export function rotate(out: Mat4, a: Mat4, rad: number, axis: Vec3): Mat4;
        export function rotateX(out: Mat4, a: Mat4, rad: number): Mat4;
        export function rotateY(out: Mat4, a: Mat4, rad: number): Mat4;
        export function rotateZ(out: Mat4, a: Mat4, rad: number): Mat4;
        export function scale(out: Mat4, a: Mat4, v: Vec3): Mat4;
        export function str(mat: Mat4): string;
        export function translate(out: Mat4, a: Mat4, v: Vec3): Mat4;
        export function transpose(out: Mat4, a: Mat4): Mat4;
    }



    // quat
    // -----------------------------------------------------------------------------

    export type Quat = Float32Array | number[];
    export module quat {
        export function add(out: Quat, a: Quat, b: Quat): Quat;
        export function calculateW(out: Quat, a: Quat): Quat;
        export function clone(a: Quat): Quat;
        export function conjugate(out: Quat, a: Quat): Quat;
        export function copy(out: Quat, a: Quat): Quat;
        export function create(): Quat;
        export function dot(a: Quat, b: Quat): number;
        export function fromMat3(out: Quat, m: Mat3): Quat;
        export function fromValues(x: number, y: number, z: number, w: number): Quat;
        export function identity(out: Quat): Quat;
        export function invert(out: Quat, a: Quat): Quat;
        export function len(a: Quat): number;
        export function length(a: Quat): number;
        export function lerp(out: Quat, a: Quat, b: Quat, t: number): Quat;
        export function mul(out: Quat, a: Quat, b: Quat): Quat;
        export function multiply(out: Quat, a: Quat, b: Quat): Quat;
        export function normalize(out: Quat, a: Quat): Quat;
        export function rotateX(out: Quat, a: Quat, rad: number): Quat;
        export function rotateY(out: Quat, a: Quat, rad: number): Quat;
        export function rotateZ(out: Quat, a: Quat, rad: number): Quat;
        export function scale(out: Quat, a: Quat, b: number): Quat;
        export function set(out: Quat, x: number, y: number, z: number, w: number): Quat;
        export function setAxisAngle(out: Quat, axis: Vec3, rad: number): Quat;
        export function slerp(out: Quat, a: Quat, b: Quat, t: number): Quat;
        export function sqrLen(a: Quat): number;
        export function squaredLength(a: Quat): number;
        export function str(a: Quat): string;
    }



    // vec2
    // -----------------------------------------------------------------------------

    export type Vec2 = Float32Array | number[];
    export module vec2 {
        export function add(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function clone(a: Vec2): Vec2;
        export function copy(out: Vec2, a: Vec2): Vec2;
        export function create(): Vec2;
        export function cross(out: Vec3, a: Vec2, b: Vec2): Vec2;
        export function dist(a: Vec2, b: Vec2): number;
        export function distance(a: Vec2, b: Vec2): number;
        export function div(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function divide(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function dot(a: Vec2, b: Vec2): number;
        export function forEach<T, ArrayType>(a: ArrayType, stride: number, offset: number, count: number, fn: (out: Vec2, a: Vec2, arg: T) => void, arg: T): ArrayType;
        export function fromValues(x: number, y: number): Vec2;
        export function len(a: Vec2): number;
        export function length(a: Vec2): number;
        export function lerp(out: Vec2, a: Vec2, b: Vec2, t: number): Vec2;
        export function max(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function min(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function mul(): Vec2;
        export function multiply(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function negate(out: Vec2, a: Vec2): Vec2;
        export function normalize(out: Vec2, a: Vec2): Vec2;
        export function random(out: Vec2, scale: number): Vec2;
        export function scale(out: Vec2, a: Vec2, b: number): Vec2;
        export function scaleAndAdd(out: Vec2, a: Vec2, b: Vec2, scale: number): Vec2;
        export function set(out: Vec2, x: number, y: number): Vec2;
        export function sqrDist(a: Vec2, b: Vec2): number;
        export function sqrLen(a: Vec2): number;
        export function squaredDistance(a: Vec2, b: Vec2): number;
        export function squaredLength(a: Vec2): number;
        export function str(a: Vec2): string;
        export function sub(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function subtract(out: Vec2, a: Vec2, b: Vec2): Vec2;
        export function transformMat2(out: Vec2, a: Vec2, m: Mat2): Vec2;
        export function transformMat2d(out: Vec2, a: Vec2, m: Mat2d): Vec2;
        export function transformMat3(out: Vec2, a: Vec2, m: Mat3): Vec2;
        export function transformMat4(out: Vec2, a: Vec2, m: Mat4): Vec2;
    }



    // vec3
    // -----------------------------------------------------------------------------

    export type Vec3 = Float32Array | number[];
    export module vec3 {
        export function add(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function clone(a: Vec3): Vec3;
        export function copy(out: Vec3, a: Vec3): Vec3;
        export function create(): Vec3;
        export function cross(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function dist(a: Vec3, b: Vec3): number;
        export function distance(a: Vec3, b: Vec3): number;
        export function div(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function divide(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function dot(a: Vec3, b: Vec3): number;
        export function forEach<T, ArrayType>(a: ArrayType, stride: number, offset: number, count: number, fn: (out: Vec3, a: Vec3, arg: T) => void, arg: T): ArrayType;
        export function fromValues(x: number, y: number, z: number): Vec3;
        export function len(a: Vec3): number;
        export function length(a: Vec3): number;
        export function lerp(out: Vec3, a: Vec3, b: Vec3, t: number): Vec3;
        export function max(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function min(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function mul(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function multiply(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function negate(out: Vec3, a: Vec3): Vec3;
        export function normalize(out: Vec3, a: Vec3): Vec3;
        export function random(out: Vec3, scale: number): Vec3;
        export function scale(out: Vec3, a: Vec3, b: number): Vec3;
        export function scaleAndAdd(out: Vec3, a: Vec3, b: Vec3, scale: number): Vec3;
        export function set(out: Vec3, x: number, y: number, z: number): Vec3;
        export function sqrDist(a: Vec3, b: Vec3): number;
        export function sqrLen(a: Vec3): number;
        export function squaredDistance(a: Vec3, b: Vec3): number;
        export function squaredLength(a: Vec3): number;
        export function str(a: Vec3): string;
        export function sub(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function subtract(out: Vec3, a: Vec3, b: Vec3): Vec3;
        export function transformMat3(out: Vec3, a: Vec3, m: Mat3): Vec3;
        export function transformMat4(out: Vec3, a: Vec3, m: Mat4): Vec3;
        export function transformQuat(out: Vec3, a: Vec3, q: Quat): Vec3;
    }



    // vec4
    // -----------------------------------------------------------------------------

    export type Vec4 = Float32Array | number[];
    export module vec4 {
        export function add(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function clone(a: Vec4): Vec4;
        export function copy(out: Vec4, a: Vec4): Vec4;
        export function create(): Vec4;
        export function dist(a: Vec4, b: Vec4): number;
        export function distance(a: Vec4, b: Vec4): number;
        export function div(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function divide(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function dot(a: Vec4, b: Vec4): number;
        export function forEach<T, ArrayType>(a: ArrayType, stride: number, offset: number, count: number, fn: (out: Vec4, a: Vec4, arg: T) => void, arg: T): ArrayType;
        export function fromValues(x: number, y: number, z: number, w: number): Vec4;
        export function len(a: Vec4): number;
        export function length(a: Vec4): number;
        export function lerp(out: Vec4, a: Vec4, b: Vec4, t: number): Vec4;
        export function max(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function min(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function mul(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function multiply(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function negate(out: Vec4, a: Vec4): Vec4;
        export function normalize(out: Vec4, a: Vec4): Vec4;
        export function random(out: Vec4, scale: number): Vec4;
        export function scale(out: Vec4, a: Vec4, b: number): Vec4;
        export function scaleAndAdd(out: Vec4, a: Vec4, b: Vec4, scale: number): Vec4;
        export function set(out: Vec4, x: number, y: number, z: number, w: number): Vec4;
        export function sqrDist(): number;
        export function sqrLen(): number;
        export function squaredDistance(a: Vec4, b: Vec4): number;
        export function squaredLength(a: Vec4): number;
        export function str(a: Vec4): string;
        export function sub(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function subtract(out: Vec4, a: Vec4, b: Vec4): Vec4;
        export function transformMat4(out: Vec4, a: Vec4, m: Mat4): Vec4;
        export function transformQuat(out: Vec4, a: Vec4, q: Quat): Vec4;
    }
}
