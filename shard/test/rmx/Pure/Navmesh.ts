/// <reference path="../../../lib/node.d.ts" />
/// <reference path="../../../lib/mocha.d.ts" />

var assert = require('assert');

import { HalfEdgeMesh } from '../../../rmx/Pure/Navmesh';



function createMeshSingleTriangle() {
    var f1, mesh, v1, v2, v3;
    mesh = new HalfEdgeMesh;
    v1 = mesh.createVertex([0, 0, 0]);
    v2 = mesh.createVertex([1, 0, 0]);
    v3 = mesh.createVertex([0, 1, 0]);
    f1 = mesh.newFace([v1, v2, v3]);
    return mesh;
}

function createMeshSingleQuad() {
    var f1, f2, mesh, v1, v2, v3, v4;
    mesh = new HalfEdgeMesh;
    v1 = mesh.createVertex([0, 0, 0]);
    v2 = mesh.createVertex([1, 0, 0]);
    v3 = mesh.createVertex([1, 1, 0]);
    v4 = mesh.createVertex([0, 1, 0]);
    f1 = mesh.newFace([v1, v2, v3]);
    f2 = mesh.newFace([v1, v3, v4]);
    return mesh;
}

function createMeshThreeTriangles() {
    var f1, f2, f3, mesh, v1, v2, v3, v4, v5;
    mesh = new HalfEdgeMesh;
    v1 = mesh.createVertex([0, 0, 0]);
    v2 = mesh.createVertex([1, 0, 0]);
    v3 = mesh.createVertex([1, 1, 0]);
    v4 = mesh.createVertex([0, 1, 0]);
    v5 = mesh.createVertex([0, -1, 0]);
    f1 = mesh.newFace([v1, v2, v3]);
    f2 = mesh.newFace([v1, v3, v4]);
    f3 = mesh.newFace([v1, v2, v5]);
    return mesh;
}

function createMeshSingleQuadSubdivided() {
    var e5, f1, f2, mesh, v1, v2, v3, v4, v5;
    mesh = new HalfEdgeMesh;
    v1 = mesh.createVertex([0, 0, 0]);
    v2 = mesh.createVertex([1, 0, 0]);
    v3 = mesh.createVertex([1, 1, 0]);
    v4 = mesh.createVertex([0, 1, 0]);
    f1 = mesh.newFace([v1, v2, v3]);
    f2 = mesh.newFace([v1, v3, v4]);
    v5 = mesh.createVertex([0.5, 0.5, 0]);
    e5 = mesh.findEdgeAt(v5.pos, 1e-6, 0.1, 0.9);
    mesh.splitEdge(e5, v5);
    return mesh;
}

function createMesh4x4Quads(holes: boolean) {
    var faces, mesh, vertices;
    mesh = new HalfEdgeMesh;
    vertices = [];
    vertices.push(null);
    vertices.push(mesh.createVertex([-1.0, -1.0, 0.0]));
    vertices.push(mesh.createVertex([-1.0, 1.0, 0.0]));
    vertices.push(mesh.createVertex([1.0, -1.0, 0.0]));
    vertices.push(mesh.createVertex([1.0, 1.0, 0.0]));
    vertices.push(mesh.createVertex([-1.0, 0.0, 0.0]));
    vertices.push(mesh.createVertex([1.0, 0.0, 0.0]));
    vertices.push(mesh.createVertex([0.0, -1.0, 0.0]));
    vertices.push(mesh.createVertex([0.0, 1.0, 0.0]));
    vertices.push(mesh.createVertex([0.0, 0.0, 0.0]));
    vertices.push(mesh.createVertex([-1.0, 0.5, 0.0]));
    vertices.push(mesh.createVertex([1.0, 0.5, 0.0]));
    vertices.push(mesh.createVertex([0.5, -1.0, 0.0]));
    vertices.push(mesh.createVertex([-0.5, 1.0, 0.0]));
    vertices.push(mesh.createVertex([-1.0, -0.5, 0.0]));
    vertices.push(mesh.createVertex([1.0, -0.5, 0.0]));
    vertices.push(mesh.createVertex([-0.5, -1.0, 0.0]));
    vertices.push(mesh.createVertex([0.5, 1.0, 0.0]));
    vertices.push(mesh.createVertex([0.5, 0.0, 0.0]));
    vertices.push(mesh.createVertex([-0.5, 0.0, 0.0]));
    vertices.push(mesh.createVertex([0.0, 0.5, 0.0]));
    vertices.push(mesh.createVertex([0.0, -0.5, 0.0]));
    vertices.push(mesh.createVertex([0.5, -0.5, 0.0]));
    vertices.push(mesh.createVertex([0.5, 0.5, 0.0]));
    vertices.push(mesh.createVertex([-0.5, 0.5, 0.0]));
    vertices.push(mesh.createVertex([-0.5, -0.5, 0.0]));
    faces = [];
    faces.push(mesh.newFace([vertices[22], vertices[12], vertices[3]]));
    faces.push(mesh.newFace([vertices[23], vertices[18], vertices[6]]));
    faces.push(mesh.newFace([vertices[24], vertices[19], vertices[9]]));
    faces.push(mesh.newFace([vertices[25], vertices[16], vertices[7]]));
    faces.push(mesh.newFace([vertices[18], vertices[22], vertices[15]]));
    faces.push(mesh.newFace([vertices[9], vertices[21], vertices[22]]));
    faces.push(mesh.newFace([vertices[21], vertices[7], vertices[12]]));
    faces.push(mesh.newFace([vertices[17], vertices[23], vertices[11]]));
    faces.push(mesh.newFace([vertices[8], vertices[20], vertices[23]]));
    if (!holes) {
        faces.push(mesh.newFace([vertices[20], vertices[9], vertices[18]]));
    }
    faces.push(mesh.newFace([vertices[13], vertices[24], vertices[20]]));
    faces.push(mesh.newFace([vertices[2], vertices[10], vertices[24]]));
    faces.push(mesh.newFace([vertices[10], vertices[5], vertices[19]]));
    if (!holes) {
        faces.push(mesh.newFace([vertices[19], vertices[25], vertices[21]]));
    }
    faces.push(mesh.newFace([vertices[5], vertices[14], vertices[25]]));
    faces.push(mesh.newFace([vertices[14], vertices[1], vertices[16]]));
    faces.push(mesh.newFace([vertices[15], vertices[22], vertices[3]]));
    faces.push(mesh.newFace([vertices[11], vertices[23], vertices[6]]));
    faces.push(mesh.newFace([vertices[20], vertices[24], vertices[9]]));
    faces.push(mesh.newFace([vertices[21], vertices[25], vertices[7]]));
    faces.push(mesh.newFace([vertices[6], vertices[18], vertices[15]]));
    faces.push(mesh.newFace([vertices[18], vertices[9], vertices[22]]));
    faces.push(mesh.newFace([vertices[22], vertices[21], vertices[12]]));
    faces.push(mesh.newFace([vertices[4], vertices[17], vertices[11]]));
    faces.push(mesh.newFace([vertices[17], vertices[8], vertices[23]]));
    faces.push(mesh.newFace([vertices[23], vertices[20], vertices[18]]));
    faces.push(mesh.newFace([vertices[8], vertices[13], vertices[20]]));
    faces.push(mesh.newFace([vertices[13], vertices[2], vertices[24]]));
    faces.push(mesh.newFace([vertices[24], vertices[10], vertices[19]]));
    faces.push(mesh.newFace([vertices[9], vertices[19], vertices[21]]));
    faces.push(mesh.newFace([vertices[19], vertices[5], vertices[25]]));
    faces.push(mesh.newFace([vertices[25], vertices[14], vertices[16]]));
    return mesh;
}

function assertArraysEqual(a1, a2, context) {
    var e, _i, _j, _len, _len1;
    for (_i = 0, _len = a1.length; _i < _len; _i++) {
        e = a1[_i];
        if (a2.indexOf(e) === -1) {
            assert.fail("incorrect element " + e + " for " + context);
        }
    }
    for (_j = 0, _len1 = a2.length; _j < _len1; _j++) {
        e = a2[_j];
        if (a1.indexOf(e) === -1) {
            assert.fail("missing element " + e + " for " + context);
        }
    }
}

function meshTests(mesh, numVertices, numEdges, numFaces, chi) {
    return function() {
        test('mesh consistency', function() {
            assert.doesNotThrow(function() {
                mesh.checkConsistency();
            });
        });

        suite('vertex.forEachOutgoingEdge', function() {
            test('vertex.forEachOutgoingEdge returns edges that starts at the vertex', function() {
                mesh.forEachVertex(function(v) {
                    v.forEachOutgoingEdge(function(e) {
                        assert.ok(e.begin === v, "v=" + v + ", e=" + e);
                    });
                });
            });
            test('vertex.forEachOutgoingEdge returns correct edges', function() {
                mesh.forEachVertex(function(v) {
                    var vertexEdges = [];
                    v.forEachOutgoingEdge(function(e) {
                        vertexEdges.push(e);
                    });
                    var bruteForceEdges = [];
                    mesh.forEachEdge(function(e) {
                        if (e.begin === v) {
                            bruteForceEdges.push(e);
                        }
                    });
                    assertArraysEqual(vertexEdges, bruteForceEdges, v);
                });
            });
        });

        suite('vertex.forEachIncomingEdge', function() {
            test('vertex.forEachIncomingEdge returns edges that end at the vertex', function() {
                mesh.forEachVertex(function(v) {
                    v.forEachIncomingEdge(function(e) {
                        assert.ok(e.end === v, "v=" + v + ", e=" + e);
                    });
                });
            });
            test('vertex.forEachIncomingEdge returns correct edges', function() {
                mesh.forEachVertex(function(v) {
                    var vertexEdges = [];
                    v.forEachIncomingEdge(function(e) {
                        vertexEdges.push(e);
                    });
                    var bruteForceEdges = [];
                    mesh.forEachEdge(function(e) {
                        if (e.end === v) {
                            bruteForceEdges.push(e);
                        }
                    });
                    assertArraysEqual(vertexEdges, bruteForceEdges, v);
                });
            });
        });

        suite('vertex.forEachNeighborVertex', function() {
            test('vertex.forEachNeighborVertex returns correct vertices', function() {
                mesh.forEachVertex(function(v) {
                    var vertexNeighbors = [];
                    v.forEachNeighborVertex(function(v2) {
                        vertexNeighbors.push(v2);
                    });
                    var bruteForceNeighbors = [];
                    mesh.forEachEdge(function(e) {
                        if (e.begin === v) {
                            bruteForceNeighbors.push(e.end);
                        }
                        if (e.end === v) {
                            bruteForceNeighbors.push(e.begin);
                        }
                    });
                    assertArraysEqual(vertexNeighbors, bruteForceNeighbors, v);
                });
            });
        });

        suite('edge.connectsVertices', function() {
            test('edge.connectsVertices is true for (e.begin, e.end)', function() {
                mesh.forEachEdge(function(e) {
                    assert.ok(e.connectsVertices(e.begin, e.end), "e=" + e);
                });
            });
            test('edge.connectsVertices is false for (e.begin, e.next.end)', function() {
                mesh.forEachEdge(function(e) {
                    assert.ok(!e.connectsVertices(e.begin, e.next.end), "e=" + e);
                });
            });
        });

        suite('edge.forEachVertex', function() {
            test('edge.forEachVertex returns 2 elements', function() {
                mesh.forEachEdge(function(e) {
                    var vertices = 0;
                    e.forEachVertex(function(v) {
                        vertices++;
                    });
                    assert.ok(vertices === 2, "e=" + e);
                });
            });
        });

        suite('edge.forEachNeighborFace', function() {
            test('edge.forEachNeighborFace returns 1-2 elements', function() {
                mesh.forEachEdge(function(e) {
                    var faces = 0;
                    e.forEachNeighborFace(function(f) {
                        faces++;
                    });
                    assert.ok((1 <= faces && faces <= 2), "e=" + e);
                });
            });
            test('edge.forEachNeighborFace consistent with isBoundary', function() {
                mesh.forEachEdge(function(e) {
                    var faces = 0;
                    e.forEachNeighborFace(function(f) {
                        faces++;
                    });
                    var expectedFaces = e.isBoundary ? 1 : 2;
                    assert.ok(faces === expectedFaces, "e=" + e);
                });
            });
        });

        suite('edge.isPointOnEdge', function() {
            test('edge.begin lies on edge', function() {
                mesh.forEachEdge(function(e) {
                    assert.ok(e.isPointOnEdge(e.begin.pos, 1e-6, 0.0, 1.0, "e=" + e));
                });
            });
            test('edge.end lies on edge', function() {
                mesh.forEachEdge(function(e) {
                    assert.ok(e.isPointOnEdge(e.end.pos, 1e-6, 0.0, 1.0, "e=" + e));
                });
            });
            test('edge.middle lies on edge', function() {
                mesh.forEachEdge(function(e) {
                    var a, b, m;
                    a = e.begin.pos;
                    b = e.end.pos;
                    m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2];
                    assert.ok(e.isPointOnEdge(m, 1e-6, 0.0, 1.0, "e=" + e));
                });
            });
            test('far point does not lie on edge', function() {
                mesh.forEachEdge(function(e) {
                    var a, b, p;
                    a = e.begin.pos;
                    b = e.end.pos;
                    p = [2 * b[0] - a[0], 2 * b[1] - a[1], 2 * b[2] - a[2]];
                    assert.ok(!e.isPointOnEdge(p, 1e-6, 0.0, 1.0, "e=" + e));
                });
            });
        });

        suite('face.forEachVertex', function() {
            test('face.forEachVertex returns face.vertexCount elements', function() {
                mesh.forEachFace(function(f) {
                    var vertices = 0;
                    f.forEachVertex(function(v) {
                        vertices++;
                    });
                    assert.ok(vertices === f.vertexCount, "f=" + f);
                });
            });
        });

        suite('face.forEachEdge', function() {
            test('face.forEachEdge returns face.vertexCount elements', function() {
                mesh.forEachFace(function(f) {
                    var edges = 0;
                    f.forEachEdge(function(e) {
                        edges++;
                    });
                    assert.ok(edges === f.vertexCount, "f=" + f);
                });
            });
            test('face.forEachEdge returns correct edges', function() {
                mesh.forEachFace(function(f) {
                    f.forEachEdge(function(e) {
                        assert.ok(e.face === f, "f=" + f + ", e=" + e);
                    });
                });
            });
        });

        suite('face.forEachNeighborFace', function() {
            test('face.forEachNeighborFace returns max. 3 elements', function() {
                mesh.forEachFace(function(f) {
                    var faces = 0;
                    f.forEachNeighborFace(function(fn) {
                        faces++;
                    });
                    assert.ok(faces <= 3, "f=" + f);
                });
            });
        });

        test('mesh.numVertices', function() {
            assert.equal(mesh.numVertices, numVertices);
        });

        test('mesh.numEdges', function() {
            assert.equal(mesh.numEdges, numEdges);
        });

        test('mesh.numFaces', function() {
            assert.equal(mesh.numFaces, numFaces);
        });

        test('mesh.getEulerCharacteristic', function() {
            assert.equal(mesh.getEulerCharacteristic(), chi);
        });

        suite('mesh.findEdge', function() {
            test('findEdge(edge.begin, edge.end) returns the edge itself', function() {
                mesh.forEachEdge(function(e) {
                    var e2 = mesh.findEdge(e.begin, e.end);
                    assert.ok(e2 === e, "e=" + e + ", e2=" + e2);
                });
            });
        });

        suite('mesh.findVertexAt', function() {
            test('findVertexAt(position of vertex) returns the vertex itself', function() {
                mesh.forEachVertex(function(v) {
                    var v2 = mesh.findVertexAt(v.pos, 1e-6);
                    assert.ok(v2 === v, "v=" + v + ", v2=" + v2);
                });
            });
            test('findVertexAt(very far position) returns nothing', function() {
                mesh.forEachVertex(function(v) {
                    var v2 = mesh.findVertexAt([1000, 1000, 1000], 1e-6);
                    assert.ok(v2 == null, "v=" + v + ", v2=" + v2);
                });
            });
        });

        suite('mesh.findEdgeAt', function() {
            test('findEdgeAt(centroid of edge) returns the edge or its twin', function() {
                mesh.forEachEdge(function(e) {
                    var a, b, e2, m;
                    a = e.begin.pos;
                    b = e.end.pos;
                    m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2];
                    e2 = mesh.findEdgeAt(m, 1e-6, 0.0, 1.0);
                    assert.ok(e2 === e || e2 === e.twin, "e=" + e + ", e2=" + e2);
                });
            });

            test('findEdgeAt(very far position) returns nothing', function() {
                mesh.forEachEdge(function(e) {
                    var e2 = mesh.findEdgeAt([1000, 1000, 1000], 1e-6, 0.0, 1.0);
                    assert.ok(e2 == null, "e=" + e + ", e2=" + e2);
                });
            });
        });
    };
}

suite("single triangle mesh", meshTests(createMeshSingleTriangle(), 3, 3, 1, 1));
suite("single quad mesh", meshTests(createMeshSingleQuad(), 4, 5, 2, 1));
suite("4x4 quad mesh", meshTests(createMesh4x4Quads(false), 25, 56, 32, 1));
suite("4x4 quad mesh with holes", meshTests(createMesh4x4Quads(true), 25, 56, 30, -1));

suite('Mesh operations', function() {
    test('mesh.findEdge works', function() {
      var e, f1, f2, mesh, v1, v2, v3, v4;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([1, 0, 0]);
      v3 = mesh.createVertex([1, 1, 0]);
      v4 = mesh.createVertex([0, 1, 0]);
      f1 = mesh.newFace([v1, v2, v3]);
      f2 = mesh.newFace([v1, v3, v4]);
      e = mesh.findEdge(v1, v3);
      assert.ok(e != null);
      assert.ok(e.connectsVertices(v1, v3));
      assert.ok(!e.connectsVertices(v3, v1));
      assert.ok(!e.connectsVertices(v1, v2));
    });
    test('mesh.splitEdge works', function() {
      var e5, f1, f2, mesh, v1, v2, v3, v4, v5;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([1, 0, 0]);
      v3 = mesh.createVertex([1, 1, 0]);
      v4 = mesh.createVertex([0, 1, 0]);
      f1 = mesh.newFace([v1, v2, v3]);
      f2 = mesh.newFace([v1, v3, v4]);
      v5 = mesh.createVertex([0.5, 0.5, 0]);
      e5 = mesh.findEdgeAt(v5.pos, 1e-6, 0.1, 0.9);
      mesh.splitEdge(e5, v5);
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
      assert.equal(mesh.numFaces, 2);
      assert.equal(mesh.numVertices, 5);
      assert.equal(mesh.numEdges, 6);
    });
    test('mesh.splitEdgeWithFaces works', function() {
      var e5, f1, f2, mesh, v1, v2, v3, v4, v5;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([1, 0, 0]);
      v3 = mesh.createVertex([1, 1, 0]);
      v4 = mesh.createVertex([0, 1, 0]);
      f1 = mesh.newFace([v1, v2, v3]);
      f2 = mesh.newFace([v1, v3, v4]);
      v5 = mesh.createVertex([0.5, 0.5, 0]);
      e5 = mesh.findEdgeAt(v5.pos, 1e-6, 0.1, 0.9);
      mesh.splitEdgeWithFaces(e5, v5);
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
      assert.equal(mesh.numFaces, 4);
      assert.equal(mesh.numVertices, 5);
      assert.equal(mesh.numEdges, 8);
    });
    test('mesh.stitchFaceBorder works', function() {
      var f1, f2, mesh, v1, v2, v3, v4, v5, v6;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([0, 2, 0]);
      v3 = mesh.createVertex([-1, 0, 0]);
      v4 = mesh.createVertex([0, 1, 0]);
      v5 = mesh.createVertex([0, 3, 0]);
      v6 = mesh.createVertex([+1, 1, 0]);
      f1 = mesh.newFace([v1, v2, v3]);
      f2 = mesh.newFace([v4, v6, v5]);
      mesh.stitchFaceBorder(f2, 1e-6, 0.01);
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
      assert.equal(mesh.numFaces, 2);
      assert.equal(mesh.numVertices, 6);
      assert.equal(mesh.numEdges, 7);
    });
    test('mesh.stitchFaceBorderNoSubdivision works', function() {
      var f1, f2, mesh, v1, v2, v3, v4, v5, v6;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([0, 2, 0]);
      v3 = mesh.createVertex([2, 0, 0]);
      v4 = mesh.createVertex([2, 0, 0]);
      v5 = mesh.createVertex([0, 2, 0]);
      v6 = mesh.createVertex([2, 2, 0]);
      f1 = mesh.newFace([v1, v2, v3]);
      f2 = mesh.newFace([v4, v5, v6]);
      mesh.stitchFaceBorderNoSubdivision(f2, function(begin, end) {
        var result;
        result = null;
        mesh.forEachEdge(function(e) {
          var d1, d2;
          d1 = e.end.distanceTo(end);
          d2 = e.begin.distanceTo(begin);
          if (d1 === 0 && d2 === 0) {
            result = e;
          }
        });
        return result;
      });
      assert.equal(mesh.numFaces, 2);
      assert.equal(mesh.numVertices, 4);
      assert.equal(mesh.numEdges, 5);
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
    });
});
suite('mesh consistency', function() {
    test('checkConsistency detects non-manifold vertex', function() {
      var f1, f2, mesh, v0, v1, v2, v3, v4;
      mesh = new HalfEdgeMesh;
      v0 = mesh.createVertex([0, 0, 0]);
      v1 = mesh.createVertex([1, 0, 0]);
      v2 = mesh.createVertex([1, 1, 0]);
      v3 = mesh.createVertex([-1, 1, 0]);
      v4 = mesh.createVertex([-1, 1, 0]);
      f1 = mesh.newFace([v0, v1, v2]);
      f2 = mesh.newFace([v0, v4, v3]);
      assert.throws((function() {
        mesh.checkConsistency();
      }), Error, "" + mesh);
    });
    test('checkConsistency detects non-manifold edge', function() {
      var f1, f2, f3, mesh, v0, v1, v2, v3, v4;
      mesh = new HalfEdgeMesh;
      v0 = mesh.createVertex([0, 0, 0]);
      v1 = mesh.createVertex([0, 1, 0]);
      v2 = mesh.createVertex([1, 0, 0]);
      v3 = mesh.createVertex([1, 1, 0]);
      v4 = mesh.createVertex([1, 2, 0]);
      f1 = mesh.newFace([v0, v1, v2]);
      f2 = mesh.newFace([v0, v1, v3]);
      f3 = mesh.newFace([v0, v1, v4]);
      assert.throws((function() {
        mesh.checkConsistency();
      }), Error, "" + mesh);
    });
    test('checkConsistency accepts isolated vertices', function() {
      var f1, mesh, v0, v1, v2, v3;
      mesh = new HalfEdgeMesh;
      v0 = mesh.createVertex([0, 0, 0]);
      v1 = mesh.createVertex([1, 0, 0]);
      v2 = mesh.createVertex([1, 1, 0]);
      v3 = mesh.createVertex([0, 0, 1]);
      f1 = mesh.newFace([v0, v1, v2]);
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
    });
    test('checkConsistency detects missing twins', function() {
      var f1, f2, mesh, v1, v2, v3, v4;
      mesh = new HalfEdgeMesh;
      v1 = mesh.createVertex([0, 0, 0]);
      v2 = mesh.createVertex([1, 0, 0]);
      v3 = mesh.createVertex([1, 1, 0]);
      v4 = mesh.createVertex([0, 1, 0]);
      f1 = mesh.newFace([v1, v2, v3], function() {});
      f2 = mesh.newFace([v1, v3, v4], function() {});
      assert.throws((function() {
        mesh.checkConsistency();
      }), Error, "" + mesh);
    });
});
