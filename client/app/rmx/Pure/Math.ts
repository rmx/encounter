// This file implements useful math functions.

module rmx.Pure {

    export function
    clamp(min: number, max: number, value: number): number {
        return Math.max(min, Math.min(max, value));
    }

    export function
    normalizeHeading(heading: number): number {
        if (heading < 0) {
            return -(-heading % (2 * Math.PI)) + 2 * Math.PI;
        } else {
            return heading % (2 * Math.PI);
        }
    }
}
