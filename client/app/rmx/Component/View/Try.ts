/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../MainNav.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../DrawerCloser.ts" />
/// <reference path="../GameSetupSiteHeader.ts" />

/// <reference path="./Game.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Core/Game.ts" />


module rmx.Component.View {

    export interface TryState {
        err    : Error;
        client : rmx.Game.Client;
    }

    class TrySpec extends ReactComponent<{}, TryState> {

        getInitialState() {
            setTimeout(() => {
                rmx.createGame
                    ( rmx.config.tutorialEncounterId
                    , rmx.Storage.Purpose.Grading
                    , (err, gameId) => {
                        if (err) {
                            this.setState({ err: err, client: null });
                        } else {
                            var client = rmx.openGame(gameId);
                            this.setState({ err: null, client: client });

                            // Automatically select the first available
                            // character class. The trial encounter should
                            // have exactly one party with exactly one
                            // character class in it.

                            var game = rmx.data.loadById<rmx.Storage.Game>(gameId);
                            game.promise.then(() => {
                                var encounter = rmx.data.loadById<rmx.Storage.Encounter>(game.content.encounter.objectId);
                                encounter.promise.then(() => {
                                    var party = encounter.content.parties[0];
                                    rmx.Game.selectRole(client, party.classes[0].toString());
                                    rmx.Game.toggleReady(client);

                                }).catch(err => {
                                    this.setState({ err: err, client: null });
                                });

                            }).catch(err => {
                                this.setState({ err: err, client: null });
                            });
                        }
                      }
                    );

            }, 200);

            return { err: null, client : null };
        }

        render() {
            if (this.state.err) {
                return Body
                    ( {}
                    , Main
                        ( { className: 'vertical' }
                        , React.DOM.h1({}, 'Ohoh, something went wrong')
                        , React.DOM.div
                            ( {}
                            , this.state.err.message
                            )
                        )
                    );

            } else if (this.state.client) {
                var client = this.state.client
                  , gameId = client.gameId;

                if (client.state.stage === rmx.Pure.Stage.Running) {
                    // FIXME: The cast?
                    return <any> rmx.Component.View.Game({ client: client });

                } else if (client.state.stage === rmx.Pure.Stage.Finished) {
                    return <any> rmx.Component.View.Game({ client: client });

                } else {
                    return Body
                        ( {}
                        , Main
                            ( { className: 'vertical' }
                            , React.DOM.h1({}, 'Preparing game... ')
                            , React.DOM.div({}, 'Game ID: ', gameId)
                            )
                        );
                }

            } else {
                return Body
                    ( {}
                    , Main
                        ( { className: 'vertical' }
                        , React.DOM.h1({}, 'Creating game...')
                        )
                    );
            }
        }
    }

    export var Try = createClass(TrySpec);
}
