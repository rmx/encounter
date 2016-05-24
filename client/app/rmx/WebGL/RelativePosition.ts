/// <reference path="../../ext/gl-matrix.ts" />

/// <reference path="../data.ts" />

/// <reference path="./Utils.ts" />
/// <reference path="./NameEntityMap.ts" />

module rmx.WebGL {

    function getEntityPosition(entity: RenderObject): Vec3 {
        if(entity.position) {
            return vec3.clone(entity.position);
        }

        var pos = vec3.fromValues(0, 0, 0);
        if(!entity.modelMatrix) {
            console.log("Error: No position and no modelMatrix found!!");
            return pos;
        }

        vec3.transformMat4(pos, pos, entity.modelMatrix);
        return pos;

    }

    // either TerrainPosition, RelativeUnitPosition or RelativeBonePosition
    interface RelativePositionType {
        getWorldCoordinates(): Vec3;
    }


    //FIXME: bone tag not tested!
    export class RelativePosition {

        public bone     : string;
        public offset   : Vec3;

        //FIXME
        // Cached entity pointer for faster access?
        // Needs to be recomputed every time the entityId member changes,
        // and every time an entity is deleted or added
        private entity   : RenderObject;
        private position : Vec3;
        private relPos   : any;
        private type     : string;

        static fromVector(position: THREE.Vector3) : RelativePosition {
            var positionStr = "abs " + position.x + " " + position.y + " " + position.z;
            return new RelativePosition(positionStr, null);
        }


        constructor(position: string, objects: any = null) {
            var pos_args  = position.split(" ");
            if(pos_args.length < 4)
                throw Error("Position requires at least 4 arguments!");

            this.entity = null;
            if(pos_args.length > 4)
                this.entity = objects[pos_args[4]];

            this.bone = "";
            if(pos_args.length > 5)
                this.bone = pos_args[5];

            this.type   = pos_args[0];
            this.offset = vec3.fromValues(parseFloat(pos_args[1]),
                                parseFloat(pos_args[2]), parseFloat(pos_args[3]));

            this.relPos   = null;
            this.position = vec3.fromValues(0.0, 0.0, 0.0);

            switch (this.type) {
            case 'abs':
                if (this.bone != "" && this.entity != null) {
                    var bpos    = new RelativeBonePosition(this.entity,
                                        this.bone, this.offset);
                    this.position = vec3.clone(bpos.getWorldCoordinates());
                } else if (this.entity != null) {
                    this.position = getEntityPosition(this.entity);
                }
                break;

            case 'rel':
                if (this.bone != "" && this.entity != null) {
                    this.relPos = new RelativeBonePosition(this.entity, this.bone, this.offset);
                } else if (this.entity != null) {
                    this.relPos = new RelativeUnitPosition(this.entity);
                }

                break;

            default:
                throw Error("Unknown position type!");
            }
        }

        getWorldCoordinates(): Vec3 {

            if (this.relPos)
                this.position = this.relPos.getWorldCoordinates();

            var resPos = vec3.clone(this.position);
            vec3.add(resPos, resPos, this.offset);
            return resPos;
        }
    }


    class RelativeUnitPosition implements RelativePositionType {

        private unit : RenderObject;

        constructor(unit: RenderObject) {
            this.unit = unit;
        }

        getWorldCoordinates(): Vec3 {
            return getEntityPosition(this.unit);
        }
    }


    class RelativeBonePosition implements RelativePositionType {

        private unit   : RenderObject;
        private mat    : Mat4;
        private offset : Vec3;
        // private pos    : Vec3;
        private vpos   : Vec3;
        // private vpos_a : Vec3;
        // private vpos_b : Vec3;
        // private vpos_c : Vec3;

        private tri_id : number;
        private tri_u  : number;
        private tri_v  : number;

        constructor(unit: RenderObject, vertex_tag: string, offset: Vec3) {

            //this.unit   = unit;
            //this.offset = vec3.copy(offset);

                //rmx.data.loadById<rmx.Storage.Model>(this.unit.appearance.modelId)
                    //.then(blob => {

                    //var model = blob.content;

                    //var vertex_tags = model.vertexTags;
                    //var vertex_id = 0;
                    //vertex_tags.forEach( x => {
                        //if (x.tag === vertex_tag) {
                            //vertex_id = parseInt(x.vertex, 10);
                            //return;
                        //}
                    //});

                    ////FIXME:
                    ///*
                    //this.unit.mesh.geometry.faces.forEach( (face, i) => {
                        //if (face.a === vertex_id ||
                            //face.b === vertex_id ||
                            //face.c === vertex_id) {
                                //this.tri_id = i;
                                //this.tri_u  = 1;
                                //this.tri_v  = 1;
                        //}
                    //});
                    //*/
                //});

                //// Some error checking
                ////if @tri_id?
                ////w = 1 - @tri_u - @tri_v
                ////if not 0 <= w <= 1
                ////throw new Error "RelativePosition: invalid barycentric coordinates (#{@tri_u}, #{@tri_v})"

                //this.mat = mat4.create();
                //this.pos = vec3.create();
                //this.vpos = vec3.create();
                //this.vpos_a = vec3.create();
                //this.vpos_b = vec3.create();
                //this.vpos_c = vec3.create();
            }

            /*
            private getUnitVertexPosition(vertex_id: number, vpos: Vec3): void  {
                var  vertex, _ref1, _ref2;
                vec3.set([0, 0, 0], vpos);
                if (this.unit == null)
                    return;

                var geometry = (_ref1 = this.unit.mesh) != null ? _ref1.geometry : void 0;
                var animation = (_ref2 = this.unit.animationState) != null ? _ref2.currentAnimation : void 0;

                if (animation != null) {
                    animation.getVertexPosition(vertex_id, vpos);
                } else if (geometry != null) {
                    vertex = geometry.vertices[vertex_id];
                    vec3.set([vertex.x, vertex.y, vertex.z], vpos);
                }
            }
            */

            private getUnitPointPosition(tri_id: number, tri_u: number,
                                         tri_v: number, vpos: Vec3): void {
                /*
                var face, geometry, mesh;
                vec3.set([0, 0, 0], vpos);
                if (this.unit == null)
                    return;

                mesh = this.unit.mesh;
                if (mesh == null)
                    return;

                geometry = mesh.geometry;
                if (geometry == null)
                    return;

                if (!((0 <= tri_id && tri_id < geometry.faces.length)))
                    return;

                face = geometry.faces[tri_id];
                this.getUnitVertexPosition(face.a, this.vpos_a);
                this.getUnitVertexPosition(face.b, this.vpos_b);
                this.getUnitVertexPosition(face.c, this.vpos_c);
                vec3.scale(this.vpos_a, tri_u);
                vec3.scale(this.vpos_b, tri_v);
                vec3.scale(this.vpos_c, 1 - tri_u - tri_v);
                vec3.add(vpos, this.vpos_a);
                vec3.add(vpos, this.vpos_b);
                vec3.add(vpos, this.vpos_c);

                // TODO: one could introduce an offset along the face normal by
                //       using face.normal
                // TODO: (pseudocode) vpos += normal_offset*face.normal
                */
            }

            getWorldMatrix(): Mat4 {
                mat4.identity(this.mat);
                mat4.translate(this.mat, this.mat, this.offset);
                var tpos = this.unit.position;
                // XXX: The last argument is a number
                // mat4.rotateZ(this.mat, this.mat, this.unit.rotation);
                if (this.tri_id != null) {
                    this.getUnitPointPosition(this.tri_id, this.tri_u,
                                            this.tri_v, this.vpos);
                    mat4.translate(this.mat, this.mat, this.vpos);
                }
                mat4.translate(this.mat, this.mat, tpos);
                return this.mat;
            }

            getWorldCoordinates(): Vec3 {
                mat4.identity(this.mat);
                var tpos = this.unit.position;
                // XXX: The last argument is a number
                // mat4.rotateZ(this.mat, this.mat, this.unit.rotation);
                vec3.set(this.vpos, 0, 0, 0);
                if (this.tri_id != null) {
                    this.getUnitPointPosition(this.tri_id, this.tri_u,
                        this.tri_v, this.vpos);
                    vec3.transformMat4(this.vpos, this.vpos, this.mat);
                }
                vec3.add(this.vpos, this.vpos, tpos);
                vec3.add(this.vpos, this.vpos, this.offset);
                return this.vpos;
            }

            //getWorldMatrix(): Mat4 {
                ////mat4.identity(this.mat);
                ////mat4.translate(this.mat, this.offset);
                ////var tpos = this.unit.position;
                ////mat4.rotateZ(this.mat, this.unit.terrainPosition.heading);
                ////if (this.tri_id != null) {
                    ////this.getUnitPointPosition(this.tri_id, this.tri_u,
                                              ////this.tri_v, this.vpos);
                    ////mat4.translate(this.mat, this.vpos);
                ////}
                ////mat4.translate(this.mat, tpos);
                ////return this.mat;
            //}

            //getWorldCoordinates(): Vec3 {
                //return vec3.createFrom(0.0, 0.0, 0.0);

                ////mat4.identity(this.mat);
                ////var tpos = this.unit.position;
                ////mat4.rotateZ(this.mat, this.unit.terrainPosition.heading);
                ////if (this.tri_id != null) {
                    ////this.getUnitPointPosition(this.tri_id, this.tri_u,
                                              ////this.tri_v, this.vpos);
                    ////mat4.multiplyVec3(this.mat, this.vpos);
                ////}
                ////vec3.add(this.vpos, tpos);
                ////vec3.add(this.vpos, this.offset);
                ////return this.vpos;
            //}
    }
}
