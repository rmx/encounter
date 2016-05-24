/// <reference path="./node.d.ts" />
/// <reference path="../app/rmx/Pure/Navmesh.ts" />

var assert = require('assert');

import HalfEdgeMesh   = rmx.Pure.HalfEdgeMesh;
import HalfEdgeVertex = rmx.Pure.HalfEdgeVertex;
import HalfEdgeEdge   = rmx.Pure.HalfEdgeEdge;
import HalfEdgeFace   = rmx.Pure.HalfEdgeFace;


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
  return {
    'mesh consistency': function() {
      assert.doesNotThrow(function() {
        mesh.checkConsistency();
      });
    },
    'vertex.forEachOutgoingEdge': {
      'vertex.forEachOutgoingEdge returns edges that starts at the vertex': function() {
        mesh.forEachVertex(function(v) {
          v.forEachOutgoingEdge(function(e) {
            assert.ok(e.begin === v, "v=" + v + ", e=" + e);
          });
        });
      },
      'vertex.forEachOutgoingEdge returns correct edges': function() {
        mesh.forEachVertex(function(v) {
          var bruteForceEdges, vertexEdges;
          vertexEdges = [];
          v.forEachOutgoingEdge(function(e) {
            vertexEdges.push(e);
          });
          bruteForceEdges = [];
          mesh.forEachEdge(function(e) {
            if (e.begin === v) {
              bruteForceEdges.push(e);
            }
          });
          assertArraysEqual(vertexEdges, bruteForceEdges, v);
        });
      }
    },
    'vertex.forEachIncomingEdge': {
      'vertex.forEachIncomingEdge returns edges that end at the vertex': function() {
        mesh.forEachVertex(function(v) {
          v.forEachIncomingEdge(function(e) {
            assert.ok(e.end === v, "v=" + v + ", e=" + e);
          });
        });
      },
      'vertex.forEachIncomingEdge returns correct edges': function() {
        mesh.forEachVertex(function(v) {
          var bruteForceEdges, vertexEdges;
          vertexEdges = [];
          v.forEachIncomingEdge(function(e) {
            vertexEdges.push(e);
          });
          bruteForceEdges = [];
          mesh.forEachEdge(function(e) {
            if (e.end === v) {
              bruteForceEdges.push(e);
            }
          });
          assertArraysEqual(vertexEdges, bruteForceEdges, v);
        });
      }
    },
    'vertex.forEachNeighborVertex': {
      'vertex.forEachNeighborVertex returns correct vertices': function() {
        mesh.forEachVertex(function(v) {
          var bruteForceNeighbors, vertexNeighbors;
          vertexNeighbors = [];
          v.forEachNeighborVertex(function(v2) {
            vertexNeighbors.push(v2);
          });
          bruteForceNeighbors = [];
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
      }
    },
    'edge.connectsVertices': {
      'edge.connectsVertices is true for (e.begin, e.end)': function() {
        mesh.forEachEdge(function(e) {
          assert.ok(e.connectsVertices(e.begin, e.end), "e=" + e);
        });
      },
      'edge.connectsVertices is false for (e.begin, e.next.end)': function() {
        mesh.forEachEdge(function(e) {
          assert.ok(!e.connectsVertices(e.begin, e.next.end), "e=" + e);
        });
      }
    },
    'edge.forEachVertex': {
      'edge.forEachVertex returns 2 elements': function() {
        mesh.forEachEdge(function(e) {
          var vertices;
          vertices = 0;
          e.forEachVertex(function(v) {
            vertices++;
          });
          assert.ok(vertices === 2, "e=" + e);
        });
      }
    },
    'edge.forEachNeighborFace': {
      'edge.forEachNeighborFace returns 1-2 elements': function() {
        mesh.forEachEdge(function(e) {
          var faces;
          faces = 0;
          e.forEachNeighborFace(function(f) {
            faces++;
          });
          assert.ok((1 <= faces && faces <= 2), "e=" + e);
        });
      },
      'edge.forEachNeighborFace consistent with isBoundary': function() {
        mesh.forEachEdge(function(e) {
          var expectedFaces, faces;
          faces = 0;
          e.forEachNeighborFace(function(f) {
            faces++;
          });
          expectedFaces = e.isBoundary ? 1 : 2;
          assert.ok(faces === expectedFaces, "e=" + e);
        });
      }
    },
    'edge.isPointOnEdge': {
      'edge.begin lies on edge': function() {
        mesh.forEachEdge(function(e) {
          assert.ok(e.isPointOnEdge(e.begin.pos, 1e-6, 0.0, 1.0, "e=" + e));
        });
      },
      'edge.end lies on edge': function() {
        mesh.forEachEdge(function(e) {
          assert.ok(e.isPointOnEdge(e.end.pos, 1e-6, 0.0, 1.0, "e=" + e));
        });
      },
      'edge.middle lies on edge': function() {
        mesh.forEachEdge(function(e) {
          var a, b, m;
          a = e.begin.pos;
          b = e.end.pos;
          m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2];
          assert.ok(e.isPointOnEdge(m, 1e-6, 0.0, 1.0, "e=" + e));
        });
      },
      'far point does not lie on edge': function() {
        mesh.forEachEdge(function(e) {
          var a, b, p;
          a = e.begin.pos;
          b = e.end.pos;
          p = [2 * b[0] - a[0], 2 * b[1] - a[1], 2 * b[2] - a[2]];
          assert.ok(!e.isPointOnEdge(p, 1e-6, 0.0, 1.0, "e=" + e));
        });
      }
    },
    'face.forEachVertex': {
      'face.forEachVertex returns face.vertexCount elements': function() {
        mesh.forEachFace(function(f) {
          var vertices;
          vertices = 0;
          f.forEachVertex(function(v) {
            vertices++;
          });
          assert.ok(vertices === f.vertexCount, "f=" + f);
        });
      }
    },
    'face.forEachEdge': {
      'face.forEachEdge returns face.vertexCount elements': function() {
        mesh.forEachFace(function(f) {
          var edges;
          edges = 0;
          f.forEachEdge(function(e) {
            edges++;
          });
          assert.ok(edges === f.vertexCount, "f=" + f);
        });
      },
      'face.forEachEdge returns correct edges': function() {
        mesh.forEachFace(function(f) {
          f.forEachEdge(function(e) {
            assert.ok(e.face === f, "f=" + f + ", e=" + e);
          });
        });
      }
    },
    'face.forEachNeighborFace': {
      'face.forEachNeighborFace returns max. 3 elements': function() {
        mesh.forEachFace(function(f) {
          var faces;
          faces = 0;
          f.forEachNeighborFace(function(fn) {
            faces++;
          });
          assert.ok(faces <= 3, "f=" + f);
        });
      }
    },
    'mesh.numVertices': function() {
      assert.equal(mesh.numVertices, numVertices);
    },
    'mesh.numEdges': function() {
      assert.equal(mesh.numEdges, numEdges);
    },
    'mesh.numFaces': function() {
      assert.equal(mesh.numFaces, numFaces);
    },
    'mesh.getEulerCharacteristic': function() {
      assert.equal(mesh.getEulerCharacteristic(), chi);
    },
    'mesh.findEdge': {
      'findEdge(edge.begin, edge.end) returns the edge itself': function() {
        mesh.forEachEdge(function(e) {
          var e2;
          e2 = mesh.findEdge(e.begin, e.end);
          assert.ok(e2 === e, "e=" + e + ", e2=" + e2);
        });
      }
    },
    'mesh.findVertexAt': {
      'findVertexAt(position of vertex) returns the vertex itself': function() {
        mesh.forEachVertex(function(v) {
          var v2;
          v2 = mesh.findVertexAt(v.pos, 1e-6);
          assert.ok(v2 === v, "v=" + v + ", v2=" + v2);
        });
      },
      'findVertexAt(very far position) returns nothing': function() {
        mesh.forEachVertex(function(v) {
          var v2;
          v2 = mesh.findVertexAt([1000, 1000, 1000], 1e-6);
          assert.ok(v2 == null, "v=" + v + ", v2=" + v2);
        });
      }
    },
    'mesh.findEdgeAt': {
      'findEdgeAt(centroid of edge) returns the edge or its twin': function() {
        mesh.forEachEdge(function(e) {
          var a, b, e2, m;
          a = e.begin.pos;
          b = e.end.pos;
          m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2];
          e2 = mesh.findEdgeAt(m, 1e-6, 0.0, 1.0);
          assert.ok(e2 === e || e2 === e.twin, "e=" + e + ", e2=" + e2);
        });
      },
      'findEdgeAt(very far position) returns nothing': function() {
        mesh.forEachEdge(function(e) {
          var e2;
          e2 = mesh.findEdgeAt([1000, 1000, 1000], 1e-6, 0.0, 1.0);
          assert.ok(e2 == null, "e=" + e + ", e2=" + e2);
        });
      }
    }
  };
};

module.exports = {
  "single triangle mesh": meshTests(createMeshSingleTriangle(), 3, 3, 1, 1),
  "single quad mesh": meshTests(createMeshSingleQuad(), 4, 5, 2, 1),
  "4x4 quad mesh": meshTests(createMesh4x4Quads(false), 25, 56, 32, 1),
  "4x4 quad mesh with holes": meshTests(createMesh4x4Quads(true), 25, 56, 30, -1),
  'mesh operations': {
    'mesh.findEdge works': function() {
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
    },
    'mesh.splitEdge works': function() {
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
    },
    'mesh.splitEdgeWithFaces works': function() {
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
    },
    'mesh.stitchFaceBorder works': function() {
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
    },
    'mesh.stitchFaceBorderNoSubdivision works': function() {
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
    }
  },
  'mesh consistency': {
    'checkConsistency detects non-manifold vertex': function() {
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
    },
    'checkConsistency detects non-manifold edge': function() {
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
    },
    'checkConsistency accepts isolated vertices': function() {
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
    },
    'checkConsistency detects missing twins': function() {
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
    }
  }
};
