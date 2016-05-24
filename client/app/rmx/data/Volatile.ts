module rmx.data {

    // Volatile
    // -----------------------------------------------------------------------
    //
    // A Volatile is a value which is read-only on the client. It may change
    // frequently, though not through the normal patch mechanism. The raw
    // value is fetched from the server through GET endpoints.

    export class Volatile<T> {

        generationNumber : number = 0;
        // ^ Generation when the value was last used. If this matches the
        // current generation it means the value is being used right now and
        // should be periodically updated.

        lastUsedAt : Date = new Date;
        // ^ The date when the value was last used. Unused values are not
        // immediately destroyed, instead they are kept around for a while
        // in case they are needed again.

        updatedAt : Date = new Date;
        // ^ Wall-clock time when the value was last updated. This is used
        // to schedule updates.

        value : T = null;
        // ^ The value. Is null initially, then set to whatever the promise
        // resolves to.

        promise : Promise<T> = null;
        // ^ If the value is being refreshed, this is the promise (as returned
        // by 'fetch').

        timeoutId : any = null;
        // ^ Timeout which will update the value sometime in the future.


        constructor
          ( public fetch : () => Promise<T>
          ) {}
    }



    // volatileValue
    // -----------------------------------------------------------------------
    //
    // Extract the value from the Volatile as a Computation.

    export function
    volatileValue<T>(v: Volatile<T>): Computation<T> {
        return new Computation<T>(() => {
            updateVolatile(v);

            if (v.value === null) {
                return Computation.Pending;

            } else {
                v.generationNumber = rmx.data.generationNumber;
                v.lastUsedAt       = new Date;

                return v.value;
            }
        });
    }



    export function
    updateVolatile<T>(v: Volatile<T>): void {
        if (v.promise === null && v.timeoutId === null) {
            v.promise = v.fetch();
            v.promise.then(value => {
                v.value = value;

            }).catch(err => {
                console.log('updateVolatile', err);

            }).then(() => {
                v.updatedAt = new Date;
                v.promise   = null;

                startNextGeneration();
                scheduleVolatileUpdate(v);
            });
        }
    }



    export function
    scheduleVolatileUpdate<T>(v: Volatile<T>): void {
        if (v.timeoutId === null) {
            var age = Date.now() - v.updatedAt.getTime();
            v.timeoutId = setTimeout(() => {
                v.timeoutId = null;
                if (rmx.data.generationNumber === v.generationNumber) {
                    updateVolatile(v);
                }
            }, 10 * 1000 - age);
        }
    }
}
