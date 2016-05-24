/// <reference path="../../../ext/gl-matrix.ts" />
/// <reference path="../../../ext/three.d.ts" />

/// <reference path="../../Pure/Ids.ts" />
/// <reference path="../../Pure/MovePath.ts" />

/// <reference path="../../WebGL/Scene.ts" />
/// <reference path="../../WebGL/RenderObject.ts" />
/// <reference path="../../WebGL/NameEntityMap.ts" />

/// <reference path="./Storage.ts" />
/// <reference path="../Game.ts" />

module rmx.Core.WebGL {

    // This module containes render functions of object which are defined in
    // the rmx.Pure module. The naming convention is: 'renderPure<TypeName>'.


    import EntityId     = rmx.Pure.EntityId;

    import Scene        = rmx.WebGL.Scene;
    import renderObject = rmx.WebGL.renderObject;
    import updateVisual = rmx.WebGL.updateVisual;

    import particleEffectObject = rmx.WebGL.particleEffectObject;


    export function
    renderPureTerrainTileInstance
    ( scene     : Scene
    , terrainId : EntityId
    , ti        : rmx.Pure.TerrainTileInstance
    ): void {
        var ro = renderObject(scene, terrainId + '/' + ti.id + '/ti');

        mat4.copy(ro.modelMatrix, ti.matProj);
        ro.model = tileModel(ti.tile.id).get(null);
    }


    export function
    renderPureTerrain
    ( scene   : Scene
    , terrain : rmx.Pure.Terrain
    ): void {
        for (var id in terrain.tileInstances) {
            var ti = terrain.tileInstances[id];
            renderPureTerrainTileInstance(scene, terrain.id, ti);
        }
    }


    export function
    renderPureHalfEdgeMesh
    ( scene     : Scene
    , terrainId : EntityId
    , navmesh   : rmx.Pure.HalfEdgeMesh
    , seqNr     : number
    ): void {
        var ro = renderObject(scene, terrainId + '/navmesh');
        updateVisual(scene, ro, '' + seqNr, () => {
            return createNavMesh(navmesh.createStaticMesh());
        });

        ro.selectable = false;
    }

    function
    createNavMesh(data: { vertices; faces; lines; }): THREE.Line {
        var geometry   = new THREE.Geometry
          , redColor   = new THREE.Color(0xff0000)
          , greenColor = new THREE.Color(0x00ffff);

        data.lines.forEach(function(l) {
            // The lines are slightly offset in +Z direction to avoid
            // z-fighting with the tile surface.
            geometry.vertices.push(new THREE.Vector3(l.v0[0], l.v0[1], l.v0[2] + 0.01));
            geometry.vertices.push(new THREE.Vector3(l.v1[0], l.v1[1], l.v1[2] + 0.01));

            var color = l.boundary ? redColor : greenColor;
            geometry.colors.push(color);
            geometry.colors.push(color);
        });

        var material = new THREE.LineBasicMaterial;
        material.vertexColors = THREE.VertexColors;

        return new THREE.Line(geometry, material, THREE.LinePieces);
    }


    export function
    renderGroundAreaEffect
    ( scene : Scene
    , gae   : rmx.Pure.GroundAreaEffect
    ): void {
        var ro = renderObject(scene, <string>gae.id);

        ro.position = vec3.clone(gae.position);
        ro.position[2] += 0.06;
        ro.rotation = vec3.fromValues(0, 0, 0);

        var aura = rmx.Core.resourceContent<rmx.Storage.Aura>(gae.auraId).get(null);
        if (aura) {
            var nem: rmx.WebGL.NameEntityMap =
                { self : ro, target : ro };

            var particleEffectId = aura.sensousEffects.auraSteady.particleId;

            if (particleEffectId) {
                var def = rmx.data.objectContent<rmx.Storage.ParticleEffect>(particleEffectId).get(null);
                if (def) {
                    particleEffectObject(
                        scene, def, nem, <string>gae.id, false);
                }
            }
        }
    }


    export function
    renderPureMovePath
    ( scene    : Scene
    , entityId : EntityId
    , movePath : rmx.Pure.MovePath
    ): void {
        var ro = renderObject(scene, entityId + '@movePath');
        updateVisual(scene, ro, movePath, () => {
            return createMovePathMesh(movePath);
        });
    }

    function
    createMovePathMesh(movePath: rmx.Pure.MovePath) {
        var geometry = new THREE.Geometry
          , material = new THREE.LineBasicMaterial
          , color    = new THREE.Color(0xFFDD44);

        movePath.path.reduce(function(v0, v1) {
            geometry.vertices.push(new THREE.Vector3(v0[0], v0[1], v0[2] + 0.01));
            geometry.vertices.push(new THREE.Vector3(v1[0], v1[1], v1[2] + 0.01));
            geometry.colors.push(color);
            geometry.colors.push(color);

            return v1;
        });

        material.vertexColors = THREE.VertexColors;

        return new THREE.Line(geometry, material, THREE.LinePieces);
    }


    export function
    renderProjectile
    ( scene      : Scene
    , projectile : rmx.Pure.Projectile
    ): void {
        var ro = renderObject(scene, <string>projectile.id);

        rmx.data.resolveReferenceString<rmx.Storage.Resource<rmx.Storage.Spell>>(
            projectile.spell.spellId).fmap(spell => {

            var nem: rmx.WebGL.NameEntityMap =
                { self : ro, target : ro};

            var particleEffectId =
                spell.content.sensousEffects.projectile.particleId;

            if(particleEffectId) {

                var spellId =
                    projectile.id + '/' + projectile.spell.id + '/projectile';

                rmx.data.findById(particleEffectId).fmap(obj => {
                    var def = <rmx.Storage.ParticleEffect>obj.content;

                    particleEffectObject(
                        scene, def, nem, spellId, false);
                }).get(null);
            }
        }).get(null);

        ro.position = vec3.clone(projectile.position);
        ro.position[2] += 1;
        ro.rotation = vec3.fromValues(0, 0, 0);
    }
}
