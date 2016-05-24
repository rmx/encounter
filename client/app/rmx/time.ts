module rmx {

    // now
    // -----------------------------------------------------------------------
    //
    // Return a high-resolution, monotonic time. The number are seconds since
    // an unspecified epoch.

    export var now : () => number = (function() {
        if (window.performance) {
            return function() { return window.performance.now() / 1000; };
        } else {
            console.warn('High-resolution clock not available.');

            var start = Date.now();
            return function() { return (Date.now() - start) / 1000; };
        }
    })();

}
