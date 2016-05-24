/// <reference path="../../Pure/Terrain.ts" />
/// <reference path="../../Pure/Game.ts" />

/// <reference path="../systems.ts" />
/// <reference path="../Entities/WorldObject.ts" />


module rmx.Game.Systems {

    import Terrain                 = rmx.Pure.Terrain;
    import lookupEntity            = rmx.Pure.lookupEntity;
    import relocateTerrainPosition = rmx.Pure.relocateTerrainPosition;


    // This system moves 'WorldObject's along their 'MovePath' if they have
    // one set.

    export class MovePath implements rmx.Game.ISystem {

        update(state: rmx.Game.State, now: number, dt: number): void {
            state.entities.forEach(entity => {
                if (entity instanceof WorldObject) {
                    var wo = <WorldObject> entity;
                    if (!wo.stunned && wo.movePath) {
                        var position = wo.movePath.moveAlong(dt, wo.movementSpeed)
                          , terrain = lookupEntity<Terrain>(state, wo.terrainPosition.terrainId);

                        relocateTerrainPosition(wo.terrainPosition, terrain, position);
                        wo.moveFlags = 1;
                        wo.terrainPosition.heading = wo.movePath.heading;

                        if (wo.movePath.done) {
                            wo.movePath  = null;
                            wo.moveFlags = 0;
                        }
                    }
                }
            });
        }
    }
}
