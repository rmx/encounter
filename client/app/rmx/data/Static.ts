module rmx.data {


    // Static<T>
    // -----------------------------------------------------------------------
    //
    // A static value which is read-only. Is loaded from the server when
    // required.

    export class Static<T> {

        status : Status = Status.Empty;
        value  : T = null;

        constructor
          ( public fetch : () => Promise<T>
          ) {}
    }



    // staticValue
    // -----------------------------------------------------------------------
    //
    // Extract the value from the Static as a Computation. If the value is not
    // loaded yet, then a request will be sent to the server to fetch it.

    export function
    staticValue<T>(s: Static<T>): Computation<T> {
        return new Computation(() => {
            loadStatic(s);

            if (s.value === null) {
                return Computation.Pending;
            } else {
                return s.value;
            }
        });
    }



    // loadStatic
    // -----------------------------------------------------------------------
    //
    // Internal function which is used to initiate the fetch if required.
    //
    // FIXME: Retry the request if the promise failed.

    function
    loadStatic<T>(s: Static<T>): void {
        if (s.status === Status.Empty) {
            s.status = Status.Loading;
            startNextGeneration();

            s.fetch().then(v => {
                s.status = Status.Loaded;
                s.value  = v;

            }).catch(err => {
                s.status = Status.Failed;

            }).then(() => {
                startNextGeneration();
            });
        }
    }
}
