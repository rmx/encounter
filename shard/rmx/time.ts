// now
// -----------------------------------------------------------------------
//
// Return the time in seconds relative to an arbitrary period.

export function now(): number {
    var hrtime = process.hrtime();
    return hrtime[0] + hrtime[1] / 1000000000;
}
