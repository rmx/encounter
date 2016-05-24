/// <reference path="../Pure/Ids.ts" />
/// <reference path="./Input.ts" />

module rmx.Core {

    import Input = rmx.Core.Input;


    // Bindings
    // -----------------------------------------------------------------------
    //
    // Bindings maintains a mapping from keys to actions which can be executed
    // to update/change a context (eg. Client, TerrainEditor etc).


    export class Bindings<T> {

        bindings : { [key: string]: IBinding };
        // ^ The available bindings which map from key to an action. This list
        // can be freely changed at runtime, for example when asynchronously
        // loading user-defined bindings from the server.

        constructor
          ( public actions : { [action: string]: ActionF<T> }
            // ^ The available actions. This is usually static, defined
            // per-context.
          ) {
            this.bindings = Object.create(null);
        }

        hasBindingFor(key: string): boolean {
            return !!this.bindings[key];
        }

        getBindingsFor(action: string): string[] {
            var keyBindings = [];
            for(var key in this.bindings) {
                if(this.bindings[key].action == action)
                    keyBindings.push(key);
            }

            return keyBindings;
        }
    }

    export interface IBinding {
        action : string;
    }

    export interface ActionF<T> {
        (context: T, keyState: KeyState): void;
    }



    // captureInputEvent
    // -----------------------------------------------------------------------
    //
    // Used by input sources to add an input event to the be processed at the
    // next update.

    export function
    captureInputEvent
    ( input    : Input
    , bindings : Bindings<any>
    , event    : Event
    ): void {
        var now = window.performance.now();
        input.events.push({ timeStamp: now, event: event });

        // Only if the key is handled by one of the bindings. This allows
        // common browser shortcuts (such as ctrl+r) through.
        if (bindings.hasBindingFor(rmx.hotkey(event))) {
            event.stopPropagation();
            event.preventDefault();
        }
    }



    // dispatchInput
    // -----------------------------------------------------------------------
    //
    // Execute binding actions for keys which were pressed during the last
    // frame.

    export function
    dispatchInput<T>
    ( now      : number
    , input    : Input
    , bindings : Bindings<T>
    , context  : T
    ): void {
        input.snapshot(now);

        for (var key in input.keys) {
            executeKeyAction(bindings, context, key, input.keys[key]);
        }

        input.reset();
    }

    function
    executeKeyAction<T>
    ( bindings : Bindings<T>
    , context  : T
    , key      : string
    , keyState: KeyState
    ): void {
        var binding = bindings.bindings[key];
        if (binding) {
            var action = bindings.actions[binding.action];
            if (action) {
                action(context, keyState);
            } else {
                console.warn('Unknown action:', binding.action);
            }
        }
    }
}
