
/// <reference path="./Utils.ts" />
/// <reference path="./RelativePosition.ts" />

/// <reference path="../../ext/gl-matrix.ts" />

module rmx.WebGL {

    export interface Zone {
        moveTo(new_pos : Vec3) : void;
        getLocation(): Vec3;
        contains(position: RelativePosition): boolean;
    }

    export function ZoneFactory(zone : Object, units: NameEntityMap,
                                offset: Vec3 = vec3.fromValues(0, 0, 0),
                                client: Object = null): Zone {

            var zoneName = Object.keys(zone)[0];
            var args     = zone[zoneName];
            var pos      = new RelativePosition(args[0], units);

            switch (zoneName) {
                case "CubeZone":
                    return new CubeZone(pos, offset, args[1], args[2], args[3]);
                case "DiscZone":
                    var norm = vec3.fromValues(args[1], args[2], args[3]);
                    return new DiscZone(pos, offset, norm, args[4], args[5]);
                case "PointZone":
                    return new PointZone(pos, offset);
                case "LineZone":
                    var dest = new RelativePosition(args[1], units);
                    return new LineZone(pos, dest, offset);
                case "SphereCapZone":
                    return new SphereCapZone(pos, offset, args[1], args[2], args[3]);
                case "EllipsoidZone":
                    return new EllipsoidZone(pos, offset, args[1], args[2], args[3]);
                case "ThreeGeometryZone":
                    return new ThreeGeometryZone(pos, offset, args[1], units, client);
                default:
                    throw Error("Unknown zone: " + zoneName);
            }
        }


    class PointZone implements Zone {
        private pos : RelativePosition;
        private offset : Vec3;

        constructor (pos: RelativePosition, offset: Vec3) {
            this.pos = pos;
            this.offset = offset;
        }

        moveTo(new_pos: Vec3): void {
            //TODO
            //this.pos.set(new_pos);
        }

        getLocation(): Vec3 {
            var pos = vec3.clone(this.pos.getWorldCoordinates());
            return vec3.add(pos, pos, this.offset);
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    class LineZone implements Zone {
        private start  : RelativePosition;
        private end    : RelativePosition;
        private offset : Vec3;

        constructor(start: RelativePosition, end: RelativePosition,
                    offset: Vec3) {
            this.start = start;
            this.end = end;
            this.offset = offset;
        }

        moveTo(new_pos: Vec3): void {
            //TODO
            //this.start.set(new_pos);
        }

        getLocation(): Vec3 {
            var dir_pos = vec3.create();
            vec3.subtract(dir_pos, this.end.getWorldCoordinates(),
                          this.start.getWorldCoordinates());
            vec3.scale(dir_pos, dir_pos, Math.random());
            vec3.add(dir_pos, dir_pos, this.start.getWorldCoordinates());
            vec3.add(dir_pos, dir_pos, this.offset);
            return dir_pos;
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    class ParallelogramZone implements Zone {

        private corner : RelativePosition;
        private offset : Vec3;
        private side1  : Vec3;
        private side2  : Vec3;

        constructor(corner: RelativePosition, offset: Vec3,
                    side1: Vec3, side2: Vec3) {
            this.corner = corner;
            this.offset = offset;
            this.side1  = vec3.clone(side1);
            this.side2  = vec3.clone(side2);
        }

        moveTo(new_pos: Vec3): void {
            //this.corner.set(new_pos);
            //TODO
        }

        getLocation(): Vec3 {
            var d1 = vec3.clone(this.side1);
            vec3.scale(d1, d1, Math.random());
            var d2 = vec3.clone(this.side2);
            vec3.scale(d2, d2, Math.random());
            vec3.add(d1, d1, d2);
            vec3.add(d1, d1, this.corner.getWorldCoordinates());
            vec3.add(d1, d1, this.offset);
            return d1;
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    class CubeZone implements Zone {

        private position : RelativePosition;
        private width    : number;
        private depth    : number;
        private height   : number;
        private offset   : Vec3;

        constructor(position: RelativePosition, offset: Vec3,
                    width: number, depth: number, height: number) {
            this.position = position;
            this.offset   = offset;
            this.width    = width;
            this.depth    = depth  != null ? depth  : this.width;
            this.height   = height != null ? height : this.width;
        }

        moveTo(new_pos: Vec3): void {
            //TODO
            //this.position.set(new_pos);
        }

        getLocation(): Vec3 {
            var location = vec3.clone(this.position.getWorldCoordinates());
            location[0] += Math.random() * this.width;
            location[1] += Math.random() * this.depth;
            location[2] += Math.random() * this.height;
            vec3.add(location, location, this.offset);
            return location;
        }

        contains(position: RelativePosition): boolean {
            var startX = this.position.getWorldCoordinates()[0] + this.offset[0];
            var startY = this.position.getWorldCoordinates()[1] + this.offset[1];
            var startZ = this.position.getWorldCoordinates()[2] + this.offset[2];
            var x = this.width;
            var y = this.depth;
            var z = this.height;

            if (x < 0) {
                startX += x;
                x = Math.abs(x);
            }
            if (y < 0) {
                startY += y;
                y = Math.abs(y);
            }
            if (z < 0) {
                startZ += z;
                z = Math.abs(z);
            }

            var diffX = position.getWorldCoordinates()[0] - startX;
            var diffY = position.getWorldCoordinates()[1] - startY;
            var diffZ = position.getWorldCoordinates()[2] - startZ;
            return (diffX > 0) && (diffX < x) && (diffY > 0) && (diffY < y) &&
                   (diffZ > 0) && (diffZ < z);
        }
    }


    export class DiscZone implements Zone {

        private center       : RelativePosition;
        private offset       : Vec3;
        private radiusNormal : Vec3;
        private outerRadius  : number;
        private innerRadius  : number;

        constructor(center: RelativePosition, offset: Vec3,
                    radiusNormal: Vec3,
                    outerRadius: number, innerRadius: number) {

            this.center       = center;
            this.offset       = offset;
            this.radiusNormal = radiusNormal;
            this.outerRadius  = outerRadius;
            this.innerRadius  = innerRadius != null ? innerRadius : 0;
        }

        moveTo(new_pos: Vec3): void {
            //TODO
            //this.center.set(new_pos);
        }

        getLocation(): Vec3 {
            var rand   = Math.random();
            var radius = this.innerRadius + (1 - rand * rand) *
                         (this.outerRadius - this.innerRadius);
            var angle  = Math.random() * ParticleEffectUtils.TWOPI;
            var x = radius * Math.cos(angle);
            var y = radius * Math.sin(angle);
            var point = vec3.fromValues(x, y, 0);

            var axis = ParticleEffectUtils.getPerpendiculars(this.radiusNormal);
            vec3.scale(axis[0], axis[0], x);
            vec3.scale(axis[1], axis[1], y);

            vec3.add(point, this.center.getWorldCoordinates(), axis[0]);
            vec3.add(point, point, axis[1]);
            vec3.add(point, point, this.offset);
            return point;
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    export class SphereCapZone implements Zone {

        private center : RelativePosition;
        private offset : Vec3;
        private minr   : number;
        private maxr   : number;
        private angle  : number;

        constructor(center: RelativePosition, offset: Vec3,
                    minr: number, maxr: number, angle: number) {
            this.center  = center;
            this.offset  = offset;
            this.minr    = minr;
            this.maxr    = maxr;
            this.angle   = angle;
            this.angle  *= ParticleEffectUtils.DEGREE_TO_RADIAN;
        }

        moveTo(new_pos: Vec3): void {
            //TODO
            //this.center.set(new_pos);
        }

        getLocation(): Vec3 {
            var theta = 2 * Math.PI * Math.random();
            var r     = this.minr - ((this.minr - this.maxr) * Math.random());
            var phi   = this.angle * Math.random();

            var v = VectorPool.get();
            v[0] = r * Math.sin(phi) * Math.cos(theta);
            v[1] = r * Math.sin(phi) * Math.sin(theta);
            v[2] = r * Math.cos(phi);
            vec3.add(v, v, this.center.getWorldCoordinates());
            vec3.add(v, v, this.offset);
            return v;
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    export class EllipsoidZone implements Zone {

        private center : RelativePosition;
        private offset : Vec3;
        private a_axis : number;
        private b_axis : number;
        private c_axis : number;

        constructor(center: RelativePosition, offset: Vec3,
                    a_axis: number, b_axis: number, c_axis: number) {
            this.center = center;
            this.offset = offset;
            this.a_axis = a_axis;
            this.b_axis = b_axis;
            this.c_axis = c_axis;
        }

        moveTo(new_pos: Vec3): void {
            //this.center.set(new_pos);
        }

        getLocation(): Vec3 {
            var u = Math.PI * Math.random() - Math.PI / 2.0;
            var v = 2 * Math.PI * Math.random() - Math.PI;

            var vv = VectorPool.get();
            vv[0] = this.a_axis * Math.cos(u) * Math.cos(v);
            vv[1] = this.b_axis * Math.cos(u) * Math.sin(v);
            vv[2] = this.c_axis * Math.sin(u);
            vec3.add(vv, vv, this.center.getWorldCoordinates());
            return vec3.add(vv, vv, this.offset);
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }


    //FIXME: re-enable
    export class ThreeGeometryZone implements Zone {

        private center : RelativePosition;
        private offset : Vec3;

        private total : number;
        private faces : any[];

        constructor(center: RelativePosition, offset: Vec3,
                    geometryId: string, units, client,
                    scale: number = 1.0) {

            this.center = center;
            this.offset = offset;

            if (scale == null)
                scale = 1.0;

            this.total = 0;
            this.faces = [];

            var obj;
            var _this = this;
            if (geometryId === "self" || geometryId === "target") {
                obj = units[geometryId];
                this.calcArea(obj.mesh.geometry);
            } else {
                client.scene.loader.loadModelGeometry(geometryId, function(err, mesh, animationLabels) {
                    if (err) {
                        return console.log("Failed to load model");
                    } else {
                        return _this.calcArea(mesh.geometry);
                    }
                });
            }
        }

        private triangleArea(a: Vec3, b: Vec3, c: Vec3): number {
            return .5 * Math.sqrt(Math.pow(b[0] * c[1] - c[0] * b[1], 2) +
                        Math.pow(c[0] * a[1] - a[0] * c[1], 2) +
                        Math.pow(a[0] * b[1] - b[0] * a[1], 2));
        }

        private calcArea(geometry: any): void {
            var ref = geometry.faces;

            var i, len;
            for (i = 0, len = ref.length; i < len; i++) {
                var face     = ref[i];
                var triangle = {
                    a: geometry.vertices[face.a],
                    b: geometry.vertices[face.b],
                    c: geometry.vertices[face.c]
                };

                var area = this.triangleArea(triangle.a, triangle.b, triangle.c);
                this.faces.push({
                    position: this.total,
                    face: triangle
                });

                this.total += area;
            }
        }

        getLocation(): Vec3 {
            var t, _i, _ref;

            var r       = Math.random() * this.total;
            var closest = null;
            var diff    = Number.POSITIVE_INFINITY;
            for (t = _i = 0, _ref = this.faces.length; 0 <= _ref ? _i < _ref : _i > _ref; t = 0 <= _ref ? ++_i : --_i) {
                var currentDiff = Math.abs(r - this.faces[t].position);
                if (currentDiff < diff) {
                    closest = t;
                    diff = currentDiff;
                }
            }

            var face = this.faces[closest];
            var b0 = Math.random();
            var b1 = (1 - b0) * Math.random();
            var b2 = 1 - b0 - b1;

            var v = VectorPool.get();
            v[0] = face.face.a.x * b0 + face.face.b.x * b1 + face.face.c.x * b2;
            v[1] = face.face.a.y * b0 + face.face.b.y * b1 + face.face.c.y * b2;
            v[2] = face.face.a.z * b0 + face.face.b.z * b1 + face.face.c.z * b2;
            vec3.add(v, v, this.center.getWorldCoordinates());
            return vec3.add(v, v, this.offset);
        }

        moveTo(new_pos: Vec3): void {
            //TODO
        }

        contains(position: RelativePosition): boolean {
            //TODO
            return false;
        }
    }
}
