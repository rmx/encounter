/// <reference path="../Core/Input.ts" />
/// <reference path="./Client.ts" />

module rmx.Game {

    import KeyState = rmx.Core.KeyState;


    // All the (game client) actions for which one can set up bindings.


    export var Actions : { [action: string]: rmx.Core.ActionF<Client> } = {};



    // Movement
    // -------------------------------------------------------------------

    Actions["move-forward"] = (client: Client, keyState: KeyState) => {
        client.localMovement.forward = keyState;
    };

    Actions["move-backward"] = (client: Client, keyState: KeyState) => {
        client.localMovement.backward = keyState;
    };

    Actions["strafe-left"] = (client: Client, keyState: KeyState) => {
        client.localMovement.strafeLeft = keyState;
    };

    Actions["strafe-right"] = (client: Client, keyState: KeyState) => {
        client.localMovement.strafeRight = keyState;
    };


    // Action buttons
    // -------------------------------------------------------------------
    //
    // The actions are executed only once the key is released.

    Actions["action-button-1"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 0); }
    };

    Actions["action-button-2"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 1); }
    };

    Actions["action-button-3"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 2); }
    };

    Actions["action-button-4"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 3); }
    };

    Actions["action-button-5"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 4); }
    };

    Actions["action-button-6"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 5); }
    };

    Actions["action-button-7"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 6); }
    };

    Actions["action-button-8"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 7); }
    };

    Actions["action-button-9"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { useActionButton(client, 8); }
    };


    // Misc
    // -------------------------------------------------------------------

    Actions["cancel-action"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) {
            var entity = client.controlledEntity;
            if (entity) {
                if (entity.spell) {
                    var msg = new rmx.Pure.Message.CMSG_SPELLCAST_ABORT();
                    client.link.sendMessage(msg);

                } else if (client.selectedAction) {
                    client.selectedAction = null;

                } else if (entity.targetId) {
                    selectEntity(client, null);
                }
            }
        }
    };

    Actions["target-nearest-enemy"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { targetNearestEnemy(client); }
    };

    Actions["screenshot"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) { takeScreenshot(client); }
    };


    // Camera
    // -------------------------------------------------------------------


    Actions["camera-zoom-in"] = (client: Client, keyState: KeyState) => {
        client.camera.changeZoom(+1);
    };

    Actions["camera-zoom-out"] = (client: Client, keyState: KeyState) => {
        client.camera.changeZoom(-1);
    };



    Actions["select"] =
    // -------------------------------------------------------------------
    //
    // Try to select the object under the mouse.

    ( client   : Client
    , keyState : KeyState
    ) => {
        //FIXME: 0 is maybe a bit too strict (units?)
        if (!keyState.pressedAt && keyState.pointerMovement[0] === 0) {
            triggerClick(client);
        }
    };



    Actions["turn-or-action"] =
    // -------------------------------------------------------------------
    //
    // Turn the character and the camera if the key was down. Otherwise
    // do something (???).

    ( client   : Client
    , keyState : KeyState
    ) => {
        var movement = client.input.pointerMovement(1);
        if (movement && movement[0] !== 0)  {
            // FIXME: and movement greater than a threshold.

            var headingDiff = 2 * Math.PI * movement[0]
              , pitchDiff   = Math.PI * movement[1];

            client.camera.changeHeading(headingDiff);
            client.camera.changePitch(pitchDiff);
            client.localMovement.lockHeading = true;

        } else if (!keyState.pressedAt && keyState.pointerMovement[0] === 0) {
            triggerClick(client);
        }
    };



    // Debug
    // -------------------------------------------------------------------

    Actions["toggle-navmesh-rendering"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) {
            client.showNavmesh = !client.showNavmesh;
        }
    };

    Actions["toggle-movepath-rendering"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) {
            client.showMovePaths = !client.showMovePaths;
        }
    };

    Actions["toggle-chrome"] = (client: Client, keyState: KeyState) => {
        if (!keyState.pressedAt) {
            client.hideChrome = !client.hideChrome;
        }
    };
}
