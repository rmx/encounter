
import { Vec3, vec3 } from 'gl-matrix';

import { State } from '../../Game/State';
import { computeWaypoints, scheduleFuture } from '../../Game';
import { WorldObject, setMovePath } from '../../Game/Entities/WorldObject';
import { MovePath } from '../../Pure/MovePath';
import { Terrain, getWorldCoordinates } from '../../Pure/Terrain';
import { lookupEntity } from '../../Pure/Game';


// The 'FollowMovementGenerator' follows a target 'WorldObject', and tries
// to remain within 'distance' to the target's boundingRadius.

export class FollowMovementGenerator {

    constructor
      ( public state    : State
      , public owner    : WorldObject
      , public target   : WorldObject
      , public distance : number
      ) {}

    initialize(): void {
        var movePath = this.computeMovePath();
        setMovePath(this.state, this.owner, movePath);

        this.scheduleUpdate();
    }

    interrupt(): void {
        // Nothing to do in the interrupt. The caller will reset the
        // movePath on the entity and any scheduled updates will
        // detect the change and stop.
    }

    update() {
        this.scheduleUpdate();

        if (this.needRecomputeMovePath()) {
            var movePath = this.computeMovePath();
            setMovePath(this.state, this.owner, movePath);
        }
    }

    private computeMovePath(): MovePath {
        var from      = this.owner.terrainPosition
          , heading   = from.heading
          , backoff   = this.distance + this.target.boundingRadius
          , waypoints = computeWaypoints(this.state, from, this.target.terrainPosition, backoff);

        return new MovePath(waypoints, heading);
    }

    private scheduleUpdate() {
        scheduleFuture(this.state, 0.5, 'updateMovementGenerator', () => {
            if (this.owner.movementGenerator == this) {
                this.update();
            }
        });
    }


    // needRecomputeMovePath
    // -------------------------------------------------------------------
    //
    // Returns true if we need to compute a new MovePath. This can be due
    // to severral reasons:
    //
    //  - The target has moved too far away from the final position since
    //    we calculated the movepath last.
    //  - The entity doesn't have a movepath anymore, possibly because it
    //    has arrived at the destination, or the path has been invalidated
    //    at some point.
    //
    // FIXME: Not entirely correct, this needs to take backoff into
    // account.

    private needRecomputeMovePath(): boolean {
        var terrain  = lookupEntity<Terrain>(this.state, this.target.terrainId)
          , tpos     = getWorldCoordinates(terrain, this.target.terrainPosition)
          , fpos     = finalPosition(this.owner)
          , distance = Math.abs(vec3.length(vec3.subtract(vec3.create(), tpos, fpos)))
          , diff     = distance - (this.distance + this.target.boundingRadius);

        return 0.1 < diff;

        function finalPosition(entity: WorldObject): Vec3 {
            if (entity.movePath) {
                return entity.movePath.finalPosition;
            } else {
                return getWorldCoordinates(terrain, entity.terrainPosition);
            }
        }
    }
}
