import { Vec3, vec3 } from 'gl-matrix';


// This was in rmx.List
export function
find<T>(list: T[], predicate: (x: T) => boolean): T {
    var index  = -1
      , length = list.length;

    while (++index < length) {
        var value = list[index];
        if (predicate(value)) {
            return value;
        }
    }
}



// Vertex
// -----------------------------------------------------------------------

export class HalfEdgeVertex {

    edge : HalfEdgeEdge;

    constructor(public id: number, public pos: Vec3) {
        this.edge = null;
    }

    get isIsolated(): boolean {
        return this.edge == null;
    }

    get isBoundary(): boolean {
        return (this.edge == null) || this.edge.isBoundary;
    }

    get isNormalized(): boolean {
        if (this.edge == null) {
            return true;
        } else if (this.edge.isBoundary) {
            return true;
        } else {
            var edge_it_end = this.edge
              , edge_it     = this.edge;

            while (edge_it) {
                edge_it = edge_it.rotateAroundEndCW;
                if (edge_it === edge_it_end) {
                    break;
                }
            }

            return edge_it != null;
        }
    }


    forEachOutgoingEdge(cb: (x: HalfEdgeEdge) => void) {
        var edge_it_end = this.edge.next
          , edge_it     = this.edge.next;

        while (true) {
            cb(edge_it);

            edge_it = edge_it.rotateAroundBeginCW;
            if (edge_it == null || edge_it === edge_it_end) {
                break;
            }
        }
    }

    forEachIncomingEdge(cb: (x: HalfEdgeEdge) => void) {
        var edge_it_end = this.edge
          , edge_it     = this.edge;

        while (true) {
            cb(edge_it);

            edge_it = edge_it.rotateAroundEndCW;
            if (edge_it == null || edge_it === edge_it_end) {
                break;
            }
        }
    }

    forEachNeighborVertex(cb: (x: HalfEdgeVertex) => void) {
        var edge_it_end = this.edge
          , edge_it     = this.edge;

        while (true) {
            cb(edge_it.begin);

            var edge_it_next = edge_it.rotateAroundEndCW;
            if (edge_it_next == null) {
                cb(edge_it.next.end);
                break;
            }

            edge_it = edge_it_next;
            if (edge_it === edge_it_end) {
                break;
            }
        }
    }


    normalizeEdge(): void {
        if (this.edge.isBoundary) {
            return;
        }

        var edge_it_begin = this.edge
          , edge_it       = this.edge;

        while (true) {
            var edge_it_next = edge_it.rotateAroundEndCCW;
            if (edge_it_next === edge_it_begin) {
                break;
            }

            if (edge_it_next != null) {
                edge_it = edge_it_next;
            } else {
                this.edge = edge_it;
                break;
            }
        }
    }


    checkConsistency() {
        if (isNaN(this.pos[0]) || isNaN(this.pos[1]) || isNaN(this.pos[2])) {
            throw new Error("Vertex: invalid position\n" + this);
        }

        if (!this.isIsolated) {
            if (this.edge.end !== this) {
                throw new Error("Vertex: edge.end != this\n" + this);
            }
            if (!this.isNormalized) {
                throw new Error("Vertex: not normalized\n" + this);
            }
            if (this.isBoundary && (this.edge.rotateAroundEndCCW != null)) {
                throw new Error("Boundary vertex: edge has unexpected CCW neighbor\n" + this);
            }
            if (!this.isBoundary && (this.edge.rotateAroundEndCCW == null)) {
                throw new Error("Inner vertex: edge has no CCW neighbor\n" + this);
            }
        }
    }

    distanceToSqr(vertex: HalfEdgeVertex): number {
        var p1 = this.pos
          , p2 = vertex.pos
          , dx = p2[0] - p1[0]
          , dy = p2[1] - p1[1]
          , dz = p2[2] - p1[2];

        return dx * dx + dy * dy + dz * dz;
    }

    distanceTo(vertex: HalfEdgeVertex): number {
        return Math.sqrt(this.distanceToSqr(vertex));
    }

    toString(): string {
        var result, _ref, _ref1, _ref2, _ref3;
        result = "HalfEdgeVertex\n{\n";
        result = result + ("    id:       " + this.id + "\n");
        result = result + ("    edge.id:  " + ((_ref = this.edge) != null ? _ref.id : void 0) + "\n");
        result = result + ("    pos:      [" + ((_ref1 = this.pos) != null ? _ref1[0] : void 0) + ", " + ((_ref2 = this.pos) != null ? _ref2[1] : void 0) + ", " + ((_ref3 = this.pos) != null ? _ref3[2] : void 0) + "]\n");
        if (!this.isNormalized) {
            result = result + "    note: vertex not normalized\n";
        }
        if (this.isIsolated) {
            result = result + "    note: vertex isolated\n";
        }
        result = result + "}\n";
        return result;
    }
}



// Edge
// -----------------------------------------------------------------------

export class HalfEdgeEdge {

    id   : number;

    prev : HalfEdgeEdge;
    next : HalfEdgeEdge;
    twin : HalfEdgeEdge;

    face : HalfEdgeFace;
    end  : HalfEdgeVertex;


    constructor() {
        this.id   = null;
        this.prev = null;
        this.next = null;
        this.twin = null;
        this.face = null;
        this.end  = null;
    }


    get begin(): HalfEdgeVertex {
        return this.prev.end;
    }

    get isBoundary(): boolean {
        return this.twin == null;
    }


    get rotateAroundBeginCW(): HalfEdgeEdge {
        return this.twin ? this.twin.next : null;
    }

    get rotateAroundBeginCCW(): HalfEdgeEdge {
        return this.prev ? this.prev.twin : null;
    }

    get rotateAroundEndCW(): HalfEdgeEdge {
        return this.next ? this.next.twin : null;
    }

    get rotateAroundEndCCW(): HalfEdgeEdge {
        return this.twin ? this.twin.prev : null;
    }


    forEachVertex(cb: (x: HalfEdgeVertex) => void): void {
        cb(this.end);
        cb(this.begin);
    }

    forEachNeighborFace(cb: (x: HalfEdgeFace) => void): void {
        cb(this.face);

        if (this.twin != null) {
            cb(this.twin.face);
        }
    }

    connectsVertices(v1: HalfEdgeVertex, v2: HalfEdgeVertex): boolean {
        // FIXME: Shouldn't this also check for begin === v2 && end === v1 ?
        return this.begin === v1 && this.end === v2;
    }

    setTwin(edgeFinder): void {
        this.twin = edgeFinder(this.end, this.begin);
        if (this.twin != null) {
            this.twin.twin = this;
        }
    }

    projectPoint(p, dest): number {
        var edge_len2, t, v, vp_x, vp_y, vp_z, vw_x, vw_y, vw_z, w;
        v = this.begin.pos;
        w = this.end.pos;
        vw_x = w[0] - v[0];
        vw_y = w[1] - v[1];
        vw_z = w[2] - v[2];
        edge_len2 = vw_x * vw_x + vw_y * vw_y + vw_z * vw_z;
        if (edge_len2 === 0) {
            dest[0] = v[0];
            dest[1] = v[1];
            dest[2] = v[2];
            return 0.5;
        }
        vp_x = p[0] - v[0];
        vp_y = p[1] - v[1];
        vp_z = p[2] - v[2];
        t = (vp_x * vw_x + vp_y * vw_y + vp_z * vw_z) / edge_len2;
        if (t < 0) {
            dest[0] = v[0];
            dest[1] = v[1];
            dest[2] = v[2];
            return 0;
        } else if (t > 1.0) {
            dest[0] = w[0];
            dest[1] = w[1];
            dest[2] = w[2];
            return 1;
        } else {
            dest[0] = v[0] + t * vw_x;
            dest[1] = v[1] + t * vw_y;
            dest[2] = v[2] + t * vw_z;
            return t;
        }
    }

    isPointOnEdge(p, tolerance, minT, maxT): boolean {
        var dist2, dx, dy, dz, p2, t;
        p2 = [0, 0, 0];
        t = this.projectPoint(p, p2);
        dx = p2[0] - p[0];
        dy = p2[1] - p[1];
        dz = p2[2] - p[2];
        dist2 = dx * dx + dy * dy + dz * dz;
        if (t < minT || t > maxT) {
            return false;
        }
        return dist2 <= tolerance * tolerance;
    }

    checkConsistency(): void {
        var _ref, _ref1;
        if (this.next == null) {
            throw new Error("Edge: no next edge\n" + this);
        }
        if (this.prev == null) {
            throw new Error("Edge: no prev edge\n" + this);
        }
        if (this.face == null) {
            throw new Error("Edge: no face\n" + this);
        }
        if (this.end == null) {
            throw new Error("Edge: no end vertex\n" + this);
        }
        if (this.begin == null) {
            throw new Error("Edge: no begin vertex\n" + this);
        }
        if (this.next.prev !== this) {
            throw new Error("Edge: next.prev != this\n" + this);
        }
        if (this.prev.next !== this) {
            throw new Error("Edge: prev.next != this\n" + this);
        }
        if ((this.twin != null) && this.twin.twin !== this) {
            throw new Error("Edge: twin.twin != this\n" + this);
        }
        if (((_ref = this.next) != null ? _ref.face : void 0) !== this.face) {
            throw new Error("Edge: next.face != this.face\n" + this);
        }
        if (((_ref1 = this.prev) != null ? _ref1.face : void 0) !== this.face) {
            throw new Error("Edge: prev.face != this.face\n" + this);
        }
        if (this.isBoundary && !this.end.isBoundary) {
            throw new Error("Edge: this.boundary -/-> end.boundary\n" + this);
        }
        if (this.isBoundary && !this.begin.isBoundary) {
            throw new Error("Edge: this.boundary -/-> begin.boundary\n" + this);
        }
        if (this.rotateAroundBeginCW === this) {
            throw new Error("Edge: this.cw == this\n" + this);
        }
        if (this.rotateAroundBeginCCW === this) {
            throw new Error("Edge: this.ccw == this\n" + this);
        }
        if (this.rotateAroundEndCW === this) {
            throw new Error("Edge: this.cw == this\n" + this);
        }
        if (this.rotateAroundEndCCW === this) {
            throw new Error("Edge: this.ccw == this\n" + this);
        }
        if ((this.rotateAroundBeginCW != null) && this.rotateAroundBeginCW.begin !== this.begin) {
            throw new Error("Edge: cw.begin != this.begin\n" + this);
        }
        if ((this.rotateAroundBeginCCW != null) && this.rotateAroundBeginCCW.begin !== this.begin) {
            throw new Error("Edge: ccw.begin != this.begin\n" + this);
        }
        if ((this.rotateAroundEndCW != null) && this.rotateAroundEndCW.end !== this.end) {
            throw new Error("Edge: cw.end != this.end\n" + this);
        }
        if ((this.rotateAroundEndCCW != null) && this.rotateAroundEndCCW.end !== this.end) {
            throw new Error("Edge: ccw.end != this.end\n" + this);
        }
    }

    toString(): string {
        var result, _ref, _ref1, _ref2, _ref3, _ref4, _ref5, _ref6;
        result = "HalfEdgeEdge\n{\n";
        result = result + ("    id:       " + this.id + "\n");
        result = result + ("    next.id:  " + ((_ref = this.next) != null ? _ref.id : void 0) + "\n");
        result = result + ("    prev.id:  " + ((_ref1 = this.prev) != null ? _ref1.id : void 0) + "\n");
        result = result + ("    twin.id:  " + ((_ref2 = this.twin) != null ? _ref2.id : void 0) + "\n");
        result = result + ("    face.id:  " + ((_ref3 = this.face) != null ? _ref3.id : void 0) + "\n");
        result = result + ("    end.id:   " + ((_ref4 = this.end) != null ? _ref4.id : void 0) + "\n");
        result = result + ("    begin.id: " + ((_ref5 = this.prev) != null ? (_ref6 = _ref5.end) != null ? _ref6.id : void 0 : void 0) + "\n");
        result = result + "}\n";
        return result;
    }
}



// Face
// -----------------------------------------------------------------------

export class HalfEdgeFace {

    id          : number;
    edge        : HalfEdgeEdge;
    vertexCount : number;
    userData    : { [key: string]: any };


    constructor() {
        this.id          = null;
        this.edge        = null;
        this.vertexCount = 0;
        this.userData    = Object.create(null);
    }

    setData(key: string, value: any): void {
        this.userData[key] = value;
    }

    getData(key: string): any {
        return this.userData[key];
    }

    setTwins(edgeFinder) {
        this.forEachEdge(function(edge) {
            edge.setTwin(edgeFinder);
        });

        this.forEachVertex(function(vertex) {
            vertex.normalizeEdge();
        });
    }

    forEachEdge(cb: (x: HalfEdgeEdge) => void) {
        var edge_it_end = this.edge
          , edge_it     = this.edge;

        while (true) {
            cb(edge_it);

            edge_it = edge_it.next;
            if (edge_it === edge_it_end) {
                break;
            }
        }
    }

    forEachVertex(cb: (x: HalfEdgeVertex) => void) {
        this.forEachEdge(function(edge) {
            cb(edge.end);
        });
    }

    forEachNeighborFace(cb: (x: HalfEdgeFace) => void) {
        this.forEachEdge(function(edge) {
            if (edge.twin) {
                cb(edge.twin.face);
            }
        });
    }

    computeCentroid(): Vec3 {
        var Axy = 0
          , Axz = 0
          , Cx  = 0
          , Cy  = 0
          , Cz  = 0;

        this.forEachEdge(function(e) {
            var v0 = e.begin.pos
              , v1 = e.end.pos
              , xy = v0[0] * v1[1] - v1[0] * v0[1]
              , xz = v0[0] * v1[2] - v1[0] * v0[2];

            Axy += xy;
            Axz += xz;
            Cx  += (v0[0] + v1[0]) * xy;
            Cy  += (v0[1] + v1[1]) * xy;
            Cz  += (v0[2] + v1[2]) * xz;
        });

        Axy = Axy / 2;
        Axz = Axz / 2;

        if (Math.abs(Axy) > 1e-6) {
            Cx = 1 / (6 * Axy) * Cx;
            Cy = 1 / (6 * Axy) * Cy;
        } else {
            Cx = this.edge.begin.pos[0];
            Cy = this.edge.begin.pos[1];
        }

        if (Math.abs(Axz) > 1e-6) {
            Cz = 1 / (6 * Axz) * Cz;
        } else {
            Cz = this.edge.begin.pos[2];
        }

        return vec3.fromValues(Cx, Cy, Cz);
    }

    checkConsistency() {
        var edge, i, _i, _ref;
        if (this.edge == null) {
            throw new Error("Face: no edge\n" + this);
        }
        if (this.edge.face !== this) {
            throw new Error("Face: edge.face != this\n" + this);
        }
        if (this.vertexCount < 3) {
            throw new Error("Face: invalid vertex count\n" + this);
        }
        edge = this.edge;
        for (i = _i = 0, _ref = this.vertexCount - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
            edge = edge.next;
        }
        if (edge !== this.edge) {
            throw new Error("Face: inconsistent vertex count\n" + this);
        }
    }

    toString(): string {
        var result, _ref;
        result = "HalfEdgeFace\n{\n";
        result = result + ("    id:       " + this.id + "\n");
        result = result + ("    edge.id:  " + ((_ref = this.edge) != null ? _ref.id : void 0) + "\n");
        result = result + ("    vertices: " + this.vertexCount + "\n");
        result = result + "}\n";
        return result;
    }
}



// Mesh
// -----------------------------------------------------------------------

export class HalfEdgeMesh {

    private idVertex : number;
    private vertices : HalfEdgeVertex[];

    private idEdge   : number;
    private edges    : HalfEdgeEdge[];

    private idFace   : number;
    private faces    : HalfEdgeFace[];


    constructor() {
        this.idVertex = 0;
        this.vertices = [];

        this.idEdge   = 0;
        this.edges    = [];

        this.idFace   = 0;
        this.faces    = [];
    }


    get numVertices(): number {
        return this.vertices.length;
    }

    get numFaces(): number {
        return this.faces.length;
    }

    get numHalfEdges(): number {
        return this.edges.length;
    }

    get numEdges(): number {
        var edges_double, edges_single;
        edges_single = 0;
        edges_double = 0;
        this.forEachEdge(function(e) {
            if (e.twin != null) {
                return edges_double++;
            } else {
                return edges_single++;
            }
        });
        return edges_single + edges_double / 2;
    }

    insertEdge(new_edge: HalfEdgeEdge): void {
        new_edge.id = this.idEdge;
        this.idEdge += 1;
        this.edges.push(new_edge);
    }

    newEdge(endVertex: HalfEdgeVertex, face: HalfEdgeFace): HalfEdgeEdge {
        var new_edge;
        new_edge = new HalfEdgeEdge;
        new_edge.face = face;
        new_edge.end = endVertex;
        this.insertEdge(new_edge);
        return new_edge;
    }

    insertFace(new_face: HalfEdgeFace): void {
        new_face.id = this.idFace;
        this.idFace += 1;
        this.faces.push(new_face);
    }

    newFace(vertices: HalfEdgeVertex[], twinFinder?): HalfEdgeFace {
        var edge, new_edge, new_edges, new_face, prev_edge, vertex, _i, _j, _len, _len1,
            _this = this;
        new_face = new HalfEdgeFace;
        this.insertFace(new_face);
        new_edges = [];
        for (_i = 0, _len = vertices.length; _i < _len; _i++) {
            vertex = vertices[_i];
            new_edge = this.newEdge(vertex, new_face);
            vertex.edge = new_edge;
            new_edges.push(new_edge);
        }
        prev_edge = new_edges[new_edges.length - 1];
        for (_j = 0, _len1 = new_edges.length; _j < _len1; _j++) {
            edge = new_edges[_j];
            edge.prev = prev_edge;
            prev_edge.next = edge;
            prev_edge = edge;
        }
        new_face.vertexCount = vertices.length;
        new_face.edge = new_edges[0];
        if (twinFinder != null) {
            new_face.setTwins(twinFinder);
        } else {
            new_face.setTwins(function(a, b) {
                return _this.findEdgeBruteForce(a, b);
            });
        }

        return new_face;
    }

    deleteVertex(vertex: HalfEdgeVertex): void {
        var index = this.vertices.indexOf(vertex);
        if (index > -1) {
            this.vertices.splice(index, 1);
        }
    }

    createVertex(position: Vec3): HalfEdgeVertex {
        var id     = this.idVertex++
          , vertex = new HalfEdgeVertex(id, position);

        this.vertices.push(vertex);

        return vertex;
    }

    forEachVertex(cb: (x: HalfEdgeVertex) => void) {
        this.vertices.forEach(cb);
    }

    forEachEdge(cb: (x: HalfEdgeEdge) => void) {
        this.edges.forEach(cb);
    }

    forEachFace(cb: (x: HalfEdgeFace) => void) {
        this.faces.forEach(cb);
    }

    findEdgeBruteForce(begin, end): HalfEdgeEdge {
        return find(this.edges, function(edge) {
            return edge.begin === begin && edge.end === end;
        });
    }

    findEdge(begin, end): HalfEdgeEdge {
        var edge = null;
        end.forEachIncomingEdge(function(e) {
            if (e.begin === begin) {
                edge = e;
            }
        });
        return edge;
    }

    findVertexAt(position: Vec3, tolerance: number): HalfEdgeVertex {
        return find(this.vertices, function(vertex) {
            var dx, dy, dz;
            dx = vertex.pos[0] - position[0];
            dy = vertex.pos[1] - position[1];
            dz = vertex.pos[2] - position[2];
            return dx * dx + dy * dy + dz * dz <= tolerance * tolerance;
        });
    }

    findEdgeAt(position, tolerance, minT, maxT): HalfEdgeEdge {
        return find(this.edges, function(edge: HalfEdgeEdge) {
            return edge.isPointOnEdge(position, tolerance, minT, maxT);
        });
    }

    normalizeAllVertices() {
        this.forEachVertex(function(v) {
            return v.normalizeEdge();
        });
    }

    mergeEdges(eA, eB) {
        var e, inEdges1, inEdges2, outEdges1, outEdges2, vA1, vA2, vB1, vB2, _i, _j, _k, _l, _len, _len1, _len2, _len3;
        vA1 = eA.begin;
        vA2 = eA.end;
        vB1 = eB.begin;
        vB2 = eB.end;
        inEdges1 = [];
        vB1.forEachIncomingEdge(function(e) {
            return inEdges1.push(e);
        });
        inEdges2 = [];
        vB2.forEachIncomingEdge(function(e) {
            return inEdges2.push(e);
        });
        outEdges1 = [];
        vB1.forEachOutgoingEdge(function(e) {
            return outEdges1.push(e);
        });
        outEdges2 = [];
        vB2.forEachOutgoingEdge(function(e) {
            return outEdges2.push(e);
        });
        for (_i = 0, _len = inEdges1.length; _i < _len; _i++) {
            e = inEdges1[_i];
            e.end = vA2;
        }
        for (_j = 0, _len1 = inEdges2.length; _j < _len1; _j++) {
            e = inEdges2[_j];
            e.end = vA1;
        }
        for (_k = 0, _len2 = outEdges1.length; _k < _len2; _k++) {
            e = outEdges1[_k];
            e.begin = vA2;
        }
        for (_l = 0, _len3 = outEdges2.length; _l < _len3; _l++) {
            e = outEdges2[_l];
            e.begin = vA1;
        }
        if (vB1 !== vA2) {
            this.deleteVertex(vB1);
        }
        if (vB2 !== vA1) {
            this.deleteVertex(vB2);
        }
        eA.twin = eB;
        eB.twin = eA;
        vA1.normalizeEdge();
        vA2.normalizeEdge();
    }

    splitEdge(edge: HalfEdgeEdge, vertex: HalfEdgeVertex) {
        var e1, e2, e3, e4, twin, vA, vB, _ref, _ref1, _ref2;
        vA = edge.begin;
        vB = edge.end;
        twin = edge.twin;
        if (twin != null) {
            edge.twin = null;
            twin.twin = null;
            _ref = this.splitHalfEdge(edge, vertex);
            e1 = _ref[0];
            e2 = _ref[1];
            _ref1 = this.splitHalfEdge(twin, vertex);
            e3 = _ref1[0];
            e4 = _ref1[1];
            e1.twin = e4;
            e2.twin = e3;
            e3.twin = e2;
            e4.twin = e1;
        } else {
            _ref2 = this.splitHalfEdge(edge, vertex); e1 = _ref2[0]; e2 = _ref2[1];
        }
        vA.normalizeEdge();
        vB.normalizeEdge();
        vertex.normalizeEdge();
        return [e1, e2, e3, e4];
    }

    splitHalfEdge(edge: HalfEdgeEdge, vertex: HalfEdgeVertex) {
        var eAB, eAD, eBY, eDB, eXA, fAB, vA, vB, vD;
        eAB = edge;
        eXA = eAB.prev;
        eBY = eAB.next;
        vA = eAB.begin;
        vB = eAB.end;
        vD = vertex;
        fAB = edge.face;
        eDB = new HalfEdgeEdge;
        eAD = eAB;
        this.insertEdge(eDB);
        eDB.end = vB;
        eAD.end = vD;
        eDB.twin = null;
        eAD.twin = null;
        eDB.face = fAB;
        eAD.face = fAB;
        eDB.next = eBY;
        eBY.prev = eDB;
        eAD.next = eDB;
        eDB.prev = eAD;
        vD.edge = eAD;
        vB.edge = eDB;
        fAB.vertexCount += 1;
        return [eAD, eDB];
    }

    splitEdgeWithFaces(edge: HalfEdgeEdge, vertex: HalfEdgeVertex) {
        var e1, e2, e3, e4, f1, f2, f3, f4, twin, vA, vB, _ref, _ref1, _ref2;
        vA = edge.begin;
        vB = edge.end;
        twin = edge.twin;
        if (twin != null) {
            _ref = this.splitHalfEdgeWithFaces(edge, vertex);
            e1 = _ref[0];
            e2 = _ref[1];
            f1 = _ref[2];
            f2 = _ref[3];
            _ref1 = this.splitHalfEdgeWithFaces(twin, vertex);
            e3 = _ref1[0];
            e4 = _ref1[1];
            f3 = _ref1[2];
            f4 = _ref1[3];
            e1.twin = e4;
            e2.twin = e3;
            e3.twin = e2;
            e4.twin = e1;
        } else {
            _ref2 = this.splitHalfEdgeWithFaces(edge, vertex);
            e1 = _ref2[0];
            e2 = _ref2[1];
            f1 = _ref2[2];
            f2 = _ref2[3];
        }
        vA.normalizeEdge();
        vB.normalizeEdge();
        vertex.normalizeEdge();
        return [e1, e2, e3, e4, f1, f2, f3, f4];
    }

    splitHalfEdgeWithFaces(edge: HalfEdgeEdge, vertex: HalfEdgeVertex) {
        var eAB, eAD, eBC, eCA, eCD, eDB, eDC, fABC, fADC, fDBC, k, v, vA, vB, vC, vD, _ref;
        if (edge.face.vertexCount !== 3) {
            throw new Error("implemented for triangles only");
        }
        eAB = edge;
        eBC = edge.next;
        eCA = edge.prev;
        vA = eCA.end;
        vB = edge.end;
        vC = eBC.end;
        vD = vertex;
        fABC = edge.face;
        eDC = new HalfEdgeEdge;
        eCD = new HalfEdgeEdge;
        eDB = new HalfEdgeEdge;
        eAD = eAB;
        fDBC = new HalfEdgeFace;
        fADC = fABC;
        _ref = fABC.userData;
        for (k in _ref) {
            v = _ref[k];
            fDBC.userData[k] = v;
        }
        this.insertEdge(eDC);
        this.insertEdge(eCD);
        this.insertEdge(eDB);
        this.insertFace(fDBC);
        eDC.end = vC;
        eDC.next = eCA;
        eDC.prev = eAD;
        eDC.face = fADC;
        eDC.twin = eCD;
        eCD.end = vD;
        eCD.next = eDB;
        eCD.prev = eBC;
        eCD.face = fDBC;
        eCD.twin = eDC;
        eDB.end = vB;
        eDB.next = eBC;
        eDB.prev = eCD;
        eDB.face = fDBC;
        eDB.twin = null;
        eAD.end = vD;
        eAD.next = eDC;
        eAD.prev = eCA;
        eAD.face = fADC;
        eAD.twin = null;
        eCA.next = eAD;
        eCA.prev = eDC;
        eCA.face = fADC;
        eBC.next = eCD;
        eBC.prev = eDB;
        eBC.face = fDBC;
        fDBC.edge = eDB;
        fDBC.vertexCount = 3;
        fADC.edge = eAD;
        vD.edge = eAD;
        vB.edge = eDB;
        vA.edge = eCA;
        return [eAD, eDB, fADC, fDBC];
    }

    stitchFaceBorder(face: HalfEdgeFace, toleranceDist, toleranceT) {
        var e, edge, mesh, new_edges, v, vertex, vertices;
        if (!(toleranceDist >= 0)) {
            throw new Error("toleranceDist must be greater or equal to zero");
        }
        if (!(toleranceT > 0)) {
            throw new Error("toleranceT must be greater than zero");
        }
        mesh = this;
        vertices = [];
        face.forEachVertex(function(v) {
            return vertices.push(v);
        });
        while (vertices.length > 0) {
            v = vertices.shift();
            edge = find(mesh.edges, function(e: HalfEdgeEdge) {
                return e.isPointOnEdge(v.pos, toleranceDist, toleranceT, 1 - toleranceT);
            });
            if ((edge != null) && edge.isBoundary) {
            new_edges = mesh.splitEdge(edge, v);
            new_edges[0].setTwin(function(a, b) {
                return mesh.findEdgeBruteForce(a, b);
            });
            new_edges[1].setTwin(function(a, b) {
                return mesh.findEdgeBruteForce(a, b);
            });
            new_edges[0].forEachVertex(function(v) {
                return v.normalizeEdge();
            });
            new_edges[1].forEachVertex(function(v) {
                return v.normalizeEdge();
            });
            }
        }
        var edges: HalfEdgeEdge[] = [];
        face.forEachEdge(function(e) {
            return edges.push(e);
        });
        while (edges.length > 0) {
            e = edges.shift();
            vertex = find(mesh.vertices, function(v: HalfEdgeVertex) {
                return e.isPointOnEdge(v.pos, toleranceDist, toleranceT, 1 - toleranceT);
            });
            if ((vertex != null) && vertex.isBoundary) {
            new_edges = mesh.splitEdge(e, vertex);
            new_edges[0].setTwin(function(a, b) {
                return mesh.findEdgeBruteForce(a, b);
            });
            new_edges[1].setTwin(function(a, b) {
                return mesh.findEdgeBruteForce(a, b);
            });
            new_edges[0].forEachVertex(function(v) {
                return v.normalizeEdge();
            });
            new_edges[1].forEachVertex(function(v) {
                return v.normalizeEdge();
            });
            edges.push(new_edges[0]);
            edges.push(new_edges[1]);
            }
        }
    }

    stitchFaceBorderNoSubdivision(face: HalfEdgeFace, edgeFinder) {
        var mesh;
        mesh = this;
        face.forEachEdge(function(edge) {
            var twinEdge;
            if (edge.isBoundary) {
            twinEdge = edgeFinder(edge.end, edge.begin);
            if (twinEdge != null) {
                mesh.mergeEdges(edge, twinEdge);
            }
            }
        });
    }

    createStaticMesh(): { vertices; faces; lines; } {
        var result =
            { vertices : []
            , faces    : []
            , lines    : null
            };

        this.forEachFace(function(f) {
            var face = [];
            f.forEachVertex(function(v) {
                result.vertices.push(v.pos);
                face.push(result.vertices.length - 1);
            });
            result.faces.push(face);
        });

        result.lines = this.createStaticLines();

        return result;
    }

    createStaticLines(): { v0; v1; boundary; }[] {
        var result = [];

        this.forEachEdge(function(e) {
            var edge = { v0: null, v1: null, boundary: false }
              , v0   = e.begin.pos
              , v1   = e.end.pos
              , c    = e.face.computeCentroid()
              , w1   = 0.95
              , w2   = (1 - w1) / 2;

            edge.v0 = [w1 * v0[0] + w2 * c[0] + w2 * v1[0], w1 * v0[1] + w2 * c[1] + w2 * v1[1], w1 * v0[2] + w2 * c[2] + w2 * v1[2]];
            edge.v1 = [w1 * v1[0] + w2 * c[0] + w2 * v0[0], w1 * v1[1] + w2 * c[1] + w2 * v0[1], w1 * v1[2] + w2 * c[2] + w2 * v0[2]];
            edge.boundary = e.isBoundary;

            result.push(edge);
        });

        return result;
    }

    getEulerCharacteristic() {
        return this.numVertices - this.numEdges + this.numFaces;
    }

    checkConsistency() {
        var _this = this;
        this.forEachVertex(function(v) {
            if (!v.isIsolated && _this.edges.indexOf(v.edge) === -1) {
                throw new Error("Mesh: vertex points to an edge that does not belong to the mesh\n" + v);
            }
        });
        this.forEachEdge(function(e) {
            if (_this.vertices.indexOf(e.end) === -1) {
                throw new Error("Mesh: edge points to an end vertex that does not belong to the mesh\n" + e);
            }
            if ((e.twin != null) && _this.edges.indexOf(e.twin) === -1) {
                throw new Error("Mesh: edge points to a twin edge that does not belong to the mesh\n" + e);
            }
            if (_this.edges.indexOf(e.prev) === -1) {
                throw new Error("Mesh: edge points to a previous edge that does not belong to the mesh\n" + e);
            }
            if (_this.edges.indexOf(e.next) === -1) {
                throw new Error("Mesh: edge points to a next edge that does not belong to the mesh\n" + e);
            }
            if (_this.faces.indexOf(e.face) === -1) {
                throw new Error("Mesh: edge points to a face that does not belong to the mesh\n" + e);
            }
        });
        this.forEachFace(function(f) {
            if (_this.edges.indexOf(f.edge) === -1) {
                throw new Error("Mesh: face points to an edge that does not belong to the mesh\n" + f);
            }
        });
        this.forEachVertex(function(v1) {
            return _this.forEachVertex(function(v2) {
                var dist2, dx, dy, dz, p1, p2;
                if (v1 === v2) {
                    return;
                }
                p1 = v1.pos;
                p2 = v2.pos;
                dx = p2[0] - p1[0];
                dy = p2[1] - p1[1];
                dz = p2[2] - p1[2];
                dist2 = dx * dx + dy * dy + dz * dz;
                if (dist2 === 0) {
                    throw new Error("Mesh: duplicate vertex\n" + v1 + "\n" + v2);
                }
            });
        });
        this.forEachEdge(function(e1) {
            return _this.forEachEdge(function(e2) {
                var e1a, e1b, e2a, e2b;
                if (e1 === e2) {
                    return;
                }
                e1a = e1.begin;
                e1b = e1.end;
                e2a = e2.begin;
                e2b = e2.end;
                if (e1a === e2a && e1b === e2b) {
                    throw new Error("Mesh: duplicate edge\n" + e1 + "\n" + e2);
                }
                if (e1a === e2b && e1b === e2a) {
                    if (e1.twin !== e2 || e2.twin !== e1) {
                        throw new Error("Mesh: missing twin for edges\n" + e1 + "\n" + e2);
                    }
                }
            });
        });
        this.forEachEdge(function(e) {
            return e.checkConsistency();
        });
        this.forEachVertex(function(v) {
            return v.checkConsistency();
        });
        this.forEachFace(function(f) {
            return f.checkConsistency();
        });
    }

    toString(level: number): string {
        var boundaryEdges, boundaryVertices, isolatedVertices, result;
        result = "HalfEdgeMesh\n{\n";
        result = result + ("    vertices: " + this.vertices.length + " (" + this.idVertex + ")\n");
        result = result + ("    edges:    " + this.edges.length + " (" + this.idEdge + ")\n");
        result = result + ("    faces:    " + this.faces.length + " (" + this.idFace + ")\n");
        if (level > 0) {
            boundaryEdges = 0;
            boundaryVertices = 0;
            isolatedVertices = 0;
            this.forEachVertex(function(v) {
                if (v.isBoundary) {
                    boundaryVertices = boundaryVertices + 1;
                }
            });
            this.forEachVertex(function(v) {
                if (v.isIsolated) {
                    isolatedVertices = isolatedVertices + 1;
                }
            });
            this.forEachEdge(function(e) {
                if (e.isBoundary) {
                    boundaryEdges = boundaryEdges + 1;
                }
            });
            result = result + ("    boundary edges:    " + boundaryEdges + "\n");
            result = result + ("    boundary vertices: " + boundaryVertices + "\n");
            result = result + ("    isolated vertices: " + isolatedVertices + "\n");
        }
        if (level > 1) {
            this.forEachVertex(function(v) {
                result = result + v.toString();
            });
            this.forEachEdge(function(e) {
                result = result + e.toString();
            });
            this.forEachFace(function(f) {
                result = result + f.toString();
            });
        }
        result = result + "}\n";
        return result;
    }
}
