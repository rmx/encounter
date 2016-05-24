import { Vec3, vec3 } from 'gl-matrix';
import { normalizeHeading } from '../Pure/Math';


export class MovePath {

    private currentPosition : Vec3;
    private currentHeading  : number;
    private waypoints       : Vec3[];


    constructor(waypoints: Vec3[], initialHeading: number) {
        this.currentPosition = vec3.clone(waypoints.shift());
        this.currentHeading  = normalizeHeading(initialHeading);
        this.waypoints       = waypoints;
    }


    get done(): boolean {
        return this.waypoints.length === 0;
    }

    // The final position of the path. The last waypoint or current position if
    // we've reached the end.
    get finalPosition(): Vec3 {
        return vec3.clone(this.waypoints[this.waypoints.length - 1] || this.currentPosition);
    }

    get path(): Vec3[] {
        return [this.currentPosition].concat(this.waypoints);
    }

    get heading(): number {
        return this.currentHeading;
    }


    moveAlong(elapsedTime: number, speed: number): Vec3 {
        // If there is no further waypoint, we've arrived at the end and we
        // always return the current position.
        if (this.done) {
            return vec3.clone(this.currentPosition);
        }

        // The vector from the current position to the next waypoint.
        var vector        = vec3.subtract(vec3.create(), this.waypoints[0], this.currentPosition)
          , vectorLength  = vec3.length(vector)
          , distanceMoved = speed * elapsedTime;

        if (vectorLength < distanceMoved) {
            // The remaining distance in the current segment is less than what
            // we moved, jump to the next waypoint and move along the next path
            // segment from there.

            vec3.copy(this.currentPosition, this.waypoints.shift());
            this.currentHeading = normalizeHeading(Math.atan2(vector[1], vector[0]));

            return this.moveAlong(elapsedTime - vectorLength / speed, speed);
        }

        // We have not yet reached the next waypoint. Move along the vector
        // towards it and update the heading.
        vec3.add(this.currentPosition, this.currentPosition, vec3.scale(vec3.create(), vec3.normalize(vec3.create(), vector), distanceMoved));
        this.currentHeading = normalizeHeading(Math.atan2(vector[1], vector[0]));

        return vec3.clone(this.currentPosition);
    }
}
