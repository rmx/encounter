/// <reference path="./Input.ts" />
/// <reference path="./Bindings.ts" />


module rmx.Core {

    // InputSource
    // -----------------------------------------------------------------------
    //
    // This class registers key and mouse events on the given node (or window)
    // and starts delivering them to the input state.

    export class InputSource {

        constructor
          ( public input    : Input
            // ^ The input state which is updated with the events.

          , public bindings : Bindings<any>
            // ^ Bindings are used to prevent the default action on key
            // events which are handled by one of the bindings.

          , public node     : HTMLElement
            // ^ The node on which mouse events are registered. This should be
            // the canvas where the scene is being rendererd.
          ) {}

        private events =
            [ 'keydown'
            , 'keyup'
            , 'mousedown'
            , 'mousemove'
            , 'mouseup'
            , 'wheel'
            , 'contextmenu'
            ];

        bindEvents(): void {
            this.events.forEach((eventName) => {
                this[eventName] = this[eventName].bind(this);

                if (eventName == 'keydown' || eventName == 'keyup') {
                    // Register key events on the window, so that we get the
                    // events regardless of which element has input focus.
                    window.addEventListener(eventName, this[eventName], false);
                } else {
                    this.node.addEventListener(eventName, this[eventName], false);
                }
            });
        }

        unbindEvents(): void {
            this.events.forEach((eventName) => {
                if (eventName == 'keydown' || eventName == 'keyup') {
                    window.removeEventListener(eventName, this[eventName]);
                } else {
                    this.node.removeEventListener(eventName, this[eventName]);
                }

                delete this[eventName];
            });
        }


        // Event handlers
        // ----------------------------------------------------------------------

        contextmenu(event: any) {
            event.preventDefault();
        }

        mousedown(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }

        mousemove(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }

        mouseup(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }

        wheel(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }

        keydown(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }

        keyup(event: any) {
            captureInputEvent(this.input, this.bindings, event);
        }
    }
}
