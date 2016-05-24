
import { State } from '../../Game/State';
import { computeWaypoints } from '../../Game';
import { WorldObject, setMovePath } from '../../Game/Entities/WorldObject';
import { MovePath } from '../../Pure/MovePath';
import { TerrainPosition } from '../../Pure/Terrain';


// The 'PointMovementGenerator' moves the entity to the given
// 'TerrainPosition'. Once there the entity stops moving.

export class PointMovementGenerator {

    constructor
      ( public state    : State
      , public entity   : WorldObject
      , public position : TerrainPosition
      , public backoff  : number
        // ^ How close the entity should come close to the given point.
      ) {}


    initialize(): void {
        var from      = this.entity.terrainPosition
          , heading   = from.heading
          , waypoints = computeWaypoints(this.state, from, this.position, this.backoff)
          , movePath  = new MovePath(waypoints, heading);

        setMovePath(this.state, this.entity, movePath);
    }

    interrupt(): void {
        // Nothing to do. The caller will reset the movePath on the entity.
    }
}
