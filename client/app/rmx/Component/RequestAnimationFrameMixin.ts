/// <reference path="../data.ts" />

module rmx.Component {

    // This mixin will call setState() repeatedly (driven by rAF) as long as
    // the component is mounted.

    export var RequestAnimationFrameMixin = {

        componentDidMount: function() {
            requestAnimationFrame(this.requestAnimationFrameUpdate);
        },

        requestAnimationFrameUpdate: function() {
            if (this.isMounted()) {
                rmx.data.startNextGeneration();
                requestAnimationFrame(this.requestAnimationFrameUpdate);
            }
        }
    };

}
