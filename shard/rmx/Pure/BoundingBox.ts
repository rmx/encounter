import { Mat4, Vec3, vec3 } from 'gl-matrix';

export class BoundingBox {

    min : Vec3;
    max : Vec3;

    constructor(...points: Vec3[]) {
        this.min = vec3.fromValues(+Infinity, +Infinity, +Infinity);
        this.max = vec3.fromValues(-Infinity, -Infinity, -Infinity);

        points.forEach(this.extend.bind(this));
    }

    clone(): BoundingBox {
        return new BoundingBox(this.min, this.max);
    }

    static fromJSON(json): BoundingBox {
        return new BoundingBox(json.min, json.max);
    }

    fromJSON(json): void {
        vec3.copy(this.min, json.min);
        vec3.copy(this.max, json.max);
    }

    toJSON() {
        return { min: [this.min[0], this.min[1], this.min[2]]
               , max: [this.max[0], this.max[1], this.max[2]]
               };
    }

    contains(point: Vec3): boolean {
        return this.containsEps(point, 0);
    }

    containsEps(point: Vec3, eps: number): boolean {
        return point[0] >= this.min[0] - eps &&
               point[0] <= this.max[0] + eps &&
               point[1] >= this.min[1] - eps &&
               point[1] <= this.max[1] + eps &&
               point[2] >= this.min[2] - eps &&
               point[2] <= this.max[2] + eps;
    }

    extend(point: Vec3): void {
        vec3.min(this.min, this.min, point);
        vec3.max(this.max, this.max, point);
    }

    extrude(delta: Vec3): void {
        vec3.add(this.max, this.max, delta);
        vec3.subtract(this.min, this.min, delta);
    }

    translate(delta: Vec3): void {
        vec3.add(this.min, this.min, delta);
        vec3.add(this.max, this.max, delta);
    }

    transform(mat: Mat4): void {
        vec3.transformMat4(this.min, this.min, mat);
        vec3.transformMat4(this.max, this.max, mat);
    }

    isEmpty(): boolean {
        return this.min[0] === +Infinity || this.min[1] === +Infinity ||
               this.min[2] === +Infinity || this.max[0] === -Infinity ||
               this.max[1] === -Infinity || this.max[2] === -Infinity;
    }

    diagonal(): Vec3 {
        return vec3.subtract(vec3.create(), this.max, this.min);
    }

    diameter(): number {
        return vec3.length(this.diagonal());
    }

    toString(): string {
        return "Min = (" + this.min[0] + ", " + this.min[1] + ", " +
               this.min[2] + ")\nMax = (" + this.max[0] + ", " +
               this.max[1] + ", " + this.max[2] + ")";
    }
}
