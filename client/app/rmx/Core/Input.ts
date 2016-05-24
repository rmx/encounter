/// <reference path="../../ext/gl-matrix.ts" />
/// <reference path="../../hotkey.ts" />

module rmx.Core {

    // Input State
    // -----------------------------------------------------------------------
    //
    // The Input class stores tracks all input events which happen during
    // a frame, and then generates a summary which is used by bindings to
    // update the context (TerrainEditor, Client, etc).
    //
    // The events are captured into a list, and then a snapshot is created
    // which digests the discrete events into a set of KeyState.

    export class Input {

        events : InputEvent<any>[] = [];
        // ^ All input events (key, mouse, wheel, ..) which have happened
        // during the last frame. Each item includes a high-resolution
        // timestamp. This list is cleared at the end of each update.


        keys : { [key: string]: KeyState } = Object.create(null);
        // ^ The digested state of keys. This is updated at the beginning of
        // each frame from the events stored in the 'events' list.


        lastPointerEvent : InputEvent<MouseEvent> = null;
        // ^ The pointer event at the end of the last frame. This is used to
        // calculate the mouse movement since the last frame.


        currentPointerEvent : MouseEvent;
        // ^ Cache used during snapshotting of the state.


        // Called at the beginning of each frame to create a snapshot of the
        // input state.
        snapshot(now: number): void {
            this.currentPointerEvent = this.lastPointerEvent ? this.lastPointerEvent.event : null;

            this.events.forEach(e => {
                switch (e.event.type) {

                case "keydown":   return this.keydown(rmx.hotkey(e.event), e.timeStamp);
                case "keyup":     return this.keyup(rmx.hotkey(e.event), e.timeStamp);

                case "wheel":     return this.wheel(e.event, e.timeStamp);

                case "mousedown": return this.mousedown(e.event, e.timeStamp);
                case "mousemove": return this.mousemove(e.event, e.timeStamp);
                case "mouseup":   return this.mouseup(e.event, e.timeStamp);

                default:
                    console.log("Unhandled event type:", e.event.type);
                }
            });

            for (var key in this.keys) {
                var keyState = this.keys[key];

                // Update the total time of pressed keys and reset the
                // pressedAt time to now, so we get an accurate timing of how
                // long the key was pressed for this and the next frame.
                if (keyState && keyState.pressedAt) {
                    keyState.totalTime += now - keyState.pressedAt;
                    keyState.pressedAt = now;
                }
            }
        }

        reset(): void {
            this.lastPointerEvent = this.events.reduce((prev, ev) => {
                if (ev.event instanceof MouseEvent) {
                    return ev;
                } else {
                    return prev;
                }
            }, this.lastPointerEvent);


            this.events = [];

            for (var key in this.keys) {
                var keyState = this.keys[key];
                if (keyState) {
                    if (keyState.pressedAt === null) {
                        delete this.keys[key];
                    } else {
                        keyState.totalTime = 0;
                    }
                }
            }
        }


        pointerMovement(button: number): Vec2 {
            var ret = vec2.fromValues(0, 0);

            this.events.reduce((prev, ev) => {
                if (ev.event instanceof MouseEvent) {
                    if (prev !== null) {
                        var x1 = offsetX(prev.event)
                          , y1 = offsetY(prev.event)
                          , x2 = offsetX(ev.event)
                          , y2 = offsetY(ev.event);

                        // FIXME: ev.event.target is not strictly correct.
                        // THis will fall apart if the events were captured
                        // on different targets (eg. multiple input sources,
                        // which we fortunately don't support (yet)).
                        var v = vec2.fromValues
                            ( (x2 - x1) / ev.event.target.clientWidth
                            , (y2 - y1) / ev.event.target.clientHeight
                            );

                        vec2.add(ret, ret, v);
                    }

                    return ev;
                } else {
                    return null;
                }
            }, this.lastPointerEvent);

            return ret;
        }

        updatePointerMovement(ev: MouseEvent): void {
            if (this.currentPointerEvent) {
                var ce = this.currentPointerEvent
                  , x1 = offsetX(ce)
                  , y1 = offsetY(ce)
                  , x2 = offsetX(ev)
                  , y2 = offsetY(ev)
                  , dx = Math.abs(x1 - x2)
                  , dy = Math.abs(y1 - y2)
                  , d  = vec2.fromValues(dx, dy);

                for (var key in this.keys) {
                    var keyState = this.keys[key];
                    if (keyState.pressedAt) {
                        vec2.add(keyState.pointerMovement, keyState.pointerMovement, d);
                    }
                }
            }

            this.currentPointerEvent = ev;
        }

        private keydown(key: string, now: number) {
            var state = this.keys[key];

            if (state) {
                if (!state.pressedAt) {
                    state.pressedAt = now;
                }
            } else {
                this.keys[key] = new KeyState(key, now);
            }
        }

        private keyup(key: string, now: number): void {
            var state = this.keys[key];

            if (state) {
                state.totalTime += now - state.pressedAt;
                state.pressedAt = null;
            }
        }

        private wheel(event: WheelEvent, now: number): void {
            if (event.deltaY !== 0) {
                var key = event.deltaY < 0 ? "wheelup" : "wheeldown";
                this.keydown(key, now);
                this.keyup(key, now);
            }
        }

        private mousedown(event: any, now: number): void {
            var key = "button-" + (event.button + 1);
            this.updatePointerMovement(event);
            this.keydown(key, now);
        }

        private mousemove(event: any, now: number): void {
            this.updatePointerMovement(event);
        }

        private mouseup(event: any, now: number): void {
            var key = "button-" + (event.button + 1);
            this.updatePointerMovement(event);
            this.keyup(key, now);
        }
    }



    // KeyState
    // -----------------------------------------------------------------------
    //
    // The KeyState represents a summary of a key (or button, encoded as
    // 'button-X') during the last frame.

    export class KeyState {

        totalTime       : number;
        // ^ The total time for how long the key has been pressed down during
        // the current frame.

        pointerMovement : Vec2;
        // ^ The absolute distance how far the pointer was moved while this
        // key was pressed down. This value is accumulated over multiple
        // frames. Can be used to determine whether the pointer has moved over
        // a certain threshold and trigger different actions based on that.
        // (ie. to distinguish between a camera move or a click).

        constructor
          ( public key       : string
          , public pressedAt : number
          ) {
            this.totalTime       = 0;
            this.pointerMovement = vec2.fromValues(0, 0);
        }
    }


    // An input event is a plain DOM event annotated with a high precision
    // precision timestamp.
    export interface InputEvent<T extends Event> {
        timeStamp : number; // DOMHighResTimeStamp
        event     : T;
    }



    export function offsetX(event: any): number {
        if (typeof event.offsetX !== 'undefined') {
            return event.offsetX;
        } else {
            return event.clientX - event.target.getBoundingClientRect().left;
        }
    }

    export function offsetY(event: any): number {
        if (typeof event.offsetY !== 'undefined') {
            return event.offsetY;
        } else {
            return event.clientY - event.target.getBoundingClientRect().top;
        }
    }
}
