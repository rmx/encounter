/// <reference path="../Entities/WorldObject.ts" />
/// <reference path="../systems.ts" />


module rmx.Game.Systems {

    import Terrain             = rmx.Pure.Terrain;
    import lookupEntity        = rmx.Pure.lookupEntity;
    import getWorldCoordinates = rmx.Pure.getWorldCoordinates;


    // This system updates the world position from the terrain position.

    export class TerrainPosition implements rmx.Game.ISystem {

        update(state: rmx.Game.State, now: number, dt: number): void {
            state.entities.forEach(entity => {
                var terrain;

                if (entity instanceof WorldObject) {
                    terrain = lookupEntity<Terrain>(state, entity.terrainPosition.terrainId);

                    entity.position = getWorldCoordinates(terrain, entity.terrainPosition);

                } else if (entity instanceof rmx.Pure.GroundAreaEffect) {
                    terrain = lookupEntity<Terrain>(state, entity.terrainPosition.terrainId);

                    entity.position = getWorldCoordinates(terrain, entity.terrainPosition);
                }
            });
        }
    }
}
