/// <reference path="../../../lib/node.d.ts" />
/// <reference path="../../../lib/mocha.d.ts" />
/// <reference path="../../../lib/gl-matrix.d.ts" />


var assert = require('assert');
import { vec3 } from 'gl-matrix';


import { Terrain, TerrainTile, TerrainPosition, TerrainTileInstance,
    TileInstanceRotation, rebuildNavmesh, isInsideTriangle,
    terrainPositionAt, relocateTerrainPosition, getWorldCoordinates,
    baryFromXY, baryToCartesian, tileInstanceAtCell, getTileNeighbors }
    from '../../../rmx/Pure/Terrain';


function flatTile() {
    return new TerrainTile("1", {
        vertices:     [0,0,0, 2,0,0, 2,2,0, 0,2,0],
        faces:        [0,1,2, 0,2,3],
    }, vec3.fromValues(1,1,1));
}

function flatTile4x2() {
    return new TerrainTile("1", {
        vertices:     [0,0,0, 8,0,0, 8,4,0, 0,4,0],
        faces:        [0,1,2, 0,2,3],
    }, vec3.fromValues(4,2,1));
}

function createTerrain() {
    var tile    = flatTile();
    var terrain = new Terrain(null, null, {}, []);

    var pos = vec3.fromValues(0,0,0);
    terrain.addTileInstance("1", tile, pos, TileInstanceRotation.R270);

    pos = vec3.fromValues(2,0,0);
    terrain.addTileInstance("2", tile, pos, TileInstanceRotation.R0);

    pos = vec3.fromValues(-2,0,0);
    terrain.addTileInstance("3", tile, pos, TileInstanceRotation.R180);

    rebuildNavmesh(terrain);

    return terrain;
}


function assertEqualish(a, b) {
    if (vec3.distance(a, b) > 0.001) {
        throw new Error('assertEqualish');
    }
}

function sampleTileContainer(): Terrain {
  var terrain = new Terrain("id", null, {}, []);

  terrain.addTileInstance("1", flatTile(), vec3.fromValues(-4, 4, 0), 0);
  terrain.addTileInstance("2", flatTile(), vec3.fromValues(-4, 0, 0), 0);
  terrain.addTileInstance("3", flatTile(), vec3.fromValues(-2, 0, 0), 0);
  terrain.addTileInstance("4", flatTile(), vec3.fromValues(0, 0, 0), 0);
  terrain.addTileInstance("5", flatTile(), vec3.fromValues(2, 0, 0), 0);
  terrain.addTileInstance("6", flatTile(), vec3.fromValues(2, 2, 0), 0);
  terrain.addTileInstance("7", flatTile(), vec3.fromValues(4, 4, 0), 0);

  return terrain;
};

suite('GridTerrainTileContainer', function() {
  suite('#getTileNeighbors', function() {
        var container: Terrain;
        setup(function() {
            container = sampleTileContainer();
        });
    test('returns no neighbors for tile at -2/2/0', function() {
      var tileInstance;
      tileInstance = tileInstanceAtCell(container, <any>[-2, 2, 0]);
      assert.ok(tileInstance);
      return assert.equal(getTileNeighbors(container, tileInstance).length, 0);
    });
    test('returns one neighbor for tile at -2/0/0', function() {
      var tileInstance;
      tileInstance = tileInstanceAtCell(container, <any>[-2, 0, 0]);
      return assert.equal(getTileNeighbors(container, tileInstance).length, 1);
    });
    test('returns 2 neighbor for tile at -1/0/0', function() {
      var tileInstance;
      tileInstance = tileInstanceAtCell(container, <any>[-1, 0, 0]);
      return assert.equal(getTileNeighbors(container, tileInstance).length, 2);
    });
    test('returns 3 neighbors for tile at 1/1/0', function() {
      var tileInstance;
      tileInstance = tileInstanceAtCell(container, <any>[1, 1, 0]);
      return assert.equal(getTileNeighbors(container, tileInstance).length, 3);
    });
    test('returns 3 neighbors for tile at 0/0/0', function() {
      var tileInstance;
      tileInstance = tileInstanceAtCell(container, <any>[0, 0, 0]);
      return assert.equal(getTileNeighbors(container, tileInstance).length, 3);
    });
  });
});

function vec3DiffLen(x1, x2): number {
  var diff;
  diff = vec3.create();
  vec3.subtract(x1, x2, diff);
  return vec3.length(diff);
}

var v0 = vec3.fromValues(-10, -10, 0);
var v1 = vec3.fromValues(-10, 10, 0);
var v2 = vec3.fromValues(10, -10, 0);

suite('Barycentric', function() {
  suite('bary3.isInsideTriangle', function() {
    test('returns true for 0/0/0', function() {
      return assert.ok(isInsideTriangle(vec3.fromValues(0, 0, 0)));
    });
    test('returns true for 1/1/1', function() {
      return assert.ok(isInsideTriangle(vec3.fromValues(1, 1, 1)));
    });
    test('returns true if vector components are all between 0 and 1', function() {
      var i, vec, _i, _results;
      _results = [];
      for (i = _i = 0; _i <= 100; i = ++_i) {
        vec = [Math.random(), Math.random(), Math.random()];
        _results.push(assert.ok(isInsideTriangle(vec), vec3.str(vec)));
      }
      return _results;
    });
    test('returns false for 2/0/0', function() {
      return assert.ok(!isInsideTriangle(vec3.fromValues(2, 0, 0)));
    });
    test('returns false for 2/2/2', function() {
      return assert.ok(!isInsideTriangle(vec3.fromValues(2, 2, 2)));
    });
  });
  test('flat quad: invertible', function() {
    var b, x1, x2;
    x1 = vec3.fromValues(-5, -5, 0);
    b = vec3.create();
    x2 = vec3.create();
    baryFromXY(v0, v1, v2, x1, b);
    baryToCartesian(v0, v1, v2, b, x2);
    return assert.ok(vec3DiffLen(x1, x2) < 0.001, vec3.str(x2));
  });
  test('flat quad: fromXY at corner', function() {
    var b1, b2, x1;
    x1 = vec3.fromValues(-10, -10, 0);
    b1 = vec3.create();
    b2 = vec3.fromValues(1, 0, 0);
    baryFromXY(v0, v1, v2, x1, b1);
    return assert.ok(vec3DiffLen(b1, b2) < 0.001, vec3.str(b1));
  });
  test('flat quad: fromXY at origin', function() {
    var b1, b2, x1;
    x1 = vec3.fromValues(0, 0, 0);
    b1 = vec3.create();
    b2 = vec3.fromValues(0, 0.5, 0.5);;
    baryFromXY(v0, v1, v2, x1, b1);
    return assert.ok(vec3DiffLen(b1, b2) < 0.001, vec3.str(b1));
  });
});

suite('Terrain', function() {
  suite('Terrain.project', function() {
    test('flat quads: origin inside', function() {
      var p, terrain, x;
      terrain = createTerrain();
      x = vec3.fromValues(0, 0, 0);
      p = terrainPositionAt(terrain, x);
      return assert.ok(p);
    });
    test('flat quads: origin inside (repeated)', function() {
      var p, terrain, x, res;
      terrain = createTerrain();
      x = vec3.fromValues(0, 0, 0);
      p = terrainPositionAt(terrain, x);
      res = relocateTerrainPosition(p, terrain, x);
      return assert.ok(res);
    });
    test('flat quads: movement', function() {
      var i, maxI, p, terrain, x1, x2, _i;
      terrain = createTerrain();
      x2 = vec3.create();
      p = terrainPositionAt(terrain, x2);
      maxI = 11;
      for (i = _i = 0; 0 <= maxI ? _i <= maxI : _i >= maxI; i = 0 <= maxI ? ++_i : --_i) {
        x1 = vec3.fromValues(-1 + 2 * i / maxI, 0, 0);
        assert.ok(relocateTerrainPosition(p, terrain, x1), 'project ' + vec3.str(x1));
        x2 = getWorldCoordinates(terrain, p);
        assertEqualish(x1, x2);
      }
    });
  });
});

suite('TerrainTile', function() {
  suite('TerrainTile.loadFromThreejs', function() {
    test('number of vertices', function() {
      var tile;
      tile = flatTile();
      return assert(tile.vertices.length, 4);
    });
    test('number of faces', function() {
      var tile;
      tile = flatTile();
      return assert(tile.projectTris.length, 2);
    });
  });
  suite('TerrainTile.cartesianXYToBarycentric', function() {
    test('flat quad: invertible', function() {
      var b, face, tile, x1, x2;
      tile = flatTile();
      b = vec3.create();
      x1 = vec3.fromValues(0, 0, 0);
      x2 = vec3.create();
      face = vec3.fromValues(0, 1, 2);
      tile.cartesianXYToBarycentric(face, x1, b);
      tile.barycentricToCartesian(face, b, x2);
      return assertEqualish(x1, x2);
    });
    test('flat quad: origin inside first triangle', function() {
      var b1, tile;
      tile = flatTile();
      b1 = vec3.create();
      tile.cartesianXYToBarycentric([0, 1, 2], [2, 1, 0], b1);
      return assertEqualish(b1, [0, 0.5, 0.5]);
    });
  });
  suite('TerrainTile.projectTri', function() {
    test('flat quad: origin inside first tri', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(0, 0, 0);
      return assert.ok(tile.projectTri(0, x, p));
    });
    test('flat quad: origin inside first tri (repeated)', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(0, 0, 0);
      tile.projectTri(0, x, p);
      return assert.ok(tile.projectTri(0, x, p));
    });
    test('flat quad: fourth vertex outside first tri', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(10, 10, 0);
      return assert.ok(!tile.projectTri(0, x, p));
    });
  });
  suite('TerrainTile.project', function() {
    test('flat quad: origin inside', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(0, 0, 0);
      return assert.ok(tile.project(p, x));
    });
    test('flat quad: origin inside (repeated)', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(0, 0, 0);
      tile.project(p, x, p);
      return assert.ok(tile.project(p, x));
    });
    test('flat quad: remote vertex outside', function() {
      var p, tile, x;
      tile = flatTile();
      p = new TerrainPosition(null);
      x = vec3.fromValues(-20, -20, 0);
      return assert.ok(!tile.project(p, x));
    });
  });
});

suite('TerrainTileInstance', function() {
  suite('A tile instance of a flat tile at 100/100/0', function() {
      var tileInstance;
      setup(function() {
          var pos = vec3.fromValues(100, 100, 0);
        tileInstance = new TerrainTileInstance("0", flatTile(), pos, TileInstanceRotation.R90);
        });
    test('projecting a point in the center', function() {
      var p, x;
      p = new TerrainPosition(null);
      x = vec3.fromValues(100.5, 100.5, 0);
      return assert.ok(tileInstance.project(p, x, p));
    });
    test('projecting a point on the left edge', function() {
      var p, x;
      p = new TerrainPosition(null);
      x = vec3.fromValues(100, 100, 0);
      return assert.ok(tileInstance.project(p, x, p));
    });
    test('projecting a point on the right edge', function() {
      var p, x;
      p = new TerrainPosition(null);
      x = vec3.fromValues(101, 101, 0);
      return assert.ok(tileInstance.project(p, x, p));
    });
    test('projecting a point outside of the tile', function() {
      var p, x;
      p = new TerrainPosition(null);
      x = vec3.fromValues(104, 104, 0);
      return assert.ok(!tileInstance.project(p, x, p));
    });
  });
  suite('A 4x2 tile instance with various positioning', function() {
    var pos = vec3.fromValues(20, 10, 0);
    test('Offset for tile at 20/10/0 with no rotation', function() {
      var ti;
      ti = new TerrainTileInstance("0", flatTile4x2(), pos, TileInstanceRotation.R0);
      assertEqualish(ti.offsetMin, [0, 0, 0]);
      return assertEqualish(ti.offsetMax, [3, 1, 0]);
    });
    test('Offset for tile at 20/10/0 with 90DEG rotation', function() {
      var ti;
      ti = new TerrainTileInstance("0", flatTile4x2(), pos, TileInstanceRotation.R90);
      assertEqualish(ti.offsetMin, [-1, 0, 0]);
      return assertEqualish(ti.offsetMax, [0, 3, 0]);
    });
    test('Offset for tile at 20/10/0 with 180DEG rotation', function() {
      var ti;
      ti = new TerrainTileInstance("0", flatTile4x2(), pos, TileInstanceRotation.R180);
      assertEqualish(ti.offsetMin, [-3, -1, 0]);
      return assertEqualish(ti.offsetMax, [0, 0, 0]);
    });
    test('Offset for tile at 20/10/0 with 270DEG rotation', function() {
      var ti;
      ti = new TerrainTileInstance("0", flatTile4x2(), pos, TileInstanceRotation.R270);
      assertEqualish(ti.offsetMin, [0, -3, 0]);
      return assertEqualish(ti.offsetMax, [1, 0, 0]);
    });
  });
});
