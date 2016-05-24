/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../RequestAnimationFrameMixin.ts" />
/// <reference path="../GameWorld.ts" />


module rmx.Component.View {

    export interface GameProps {
        client : rmx.Game.Client;
    }

    class GameSpec extends ReactComponent<GameProps, {}> {
        render() {
            var client = this.props.client
              , state  = client.state;

            var siteHeader = GameSetupSiteHeader({ client: client });

            if (client.error) {
                return Body
                    ( {}
                    , siteHeader
                    , Exception( { error: client.error })
                    );

            } else if (state.encounterId) {

                function loadingScreen() {
                    return Body
                        ( {}
                        , LoadingScreen()
                        );
                }

                function errorScreen() {
                    return loadingScreen(); // XXX
                }


                function renderGame(entity): any { // XXX: Return type?
                    if (entity == Computation.Pending) {
                        return loadingScreen();

                    } else {
                        if (client.state.stage == rmx.Pure.Stage.Setup) {
                            return GameSetupBody
                                ( { client: client }
                                , GameSetupBanner({ client: client })
                                , React.DOM.div
                                    ( { className: 'rmx game-setup-main-encounter' }
                                    , 'Encounter details here'
                                    )
                                );

                        } else if (client.state.stage == rmx.Pure.Stage.Running) {
                            if (client.controlledEntityId) {
                                return GameWorld({ client: client });

                            } else {
                                return loadingScreen();
                            }

                        } else if (client.state.stage == rmx.Pure.Stage.Finished) {
                            //client.renderer.gameOver();
                            //XXX: For now we stay still render the GUI and world
                            return GameWorld({ client: client });

                        } else {
                            return errorScreen();
                        }
                    }
                }

                return rmx.data
                    .findById(state.encounterId)
                    .then(renderGame)
                    .get(errorScreen());

            } else {
                return loadingScreen();
            }
        }
    }

    GameSpec.prototype.mixins =
        [ RequestAnimationFrameMixin
        ];

    export var Game = createClass(GameSpec);
}
