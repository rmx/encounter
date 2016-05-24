/// <reference path="./Base.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./Link.ts" />

/// <reference path="../data.ts" />
/// <reference path="../Core/Encounter.ts" />
/// <reference path="../Game/Client.ts" />

module rmx.Component {

    export interface GameSetupBannerProps {
        client : rmx.Game.Client;
    }

    class GameSetupBannerSpec extends ReactComponent<GameSetupBannerProps, {}> {

        render() {
            var client = this.props.client
              , player = rmx.Pure.lookupPlayer(client.state, rmx.data.session.accountId);

            if (client.state.stage == rmx.Pure.Stage.Running) {
                var gameHref = '/games/' + client.state.gameId;

                return React.DOM.div
                    ( { className: 'rmx game-setup-banner' }
                    , React.DOM.i({ className: 'check icon' })
                    , React.DOM.div
                        ( { className: 'content' }
                        , React.DOM.div({ className: 'label' }, 'The game has started')
                        , React.DOM.div
                            ( { className: 'sub' }
                            , Link({ href: gameHref }, 'Click here to enter the world.')
                            )
                        )
                    );

            } else if (player && player.ready) {
                return React.DOM.div
                    ( { className: 'rmx game-setup-banner' }
                    , React.DOM.i({ className: 'check icon' })
                    , React.DOM.div
                        ( { className: 'content' }
                        , React.DOM.div({ className: 'label' }, 'You are ready to begin the game')
                        , React.DOM.div
                            ( { className: 'sub' }
                            , 'Unfortunately, some of the other players aren\'t ready yet. So we have to wait for them.  You can talk to them and help them with their character class selection in the strategy room. The game will start automatically once all players are ready.'
                            )
                        )
                    );

            } else if (player && player.roleId) {
                return React.DOM.div
                    ( { className: 'rmx game-setup-banner' }
                    , React.DOM.i({ className: 'check icon' })
                    , React.DOM.div
                        ( { className: 'content' }
                        , React.DOM.div
                            ( { className: 'label' }
                            , 'Are you ready to begin the game?'
                            , React.DOM.button
                                ( { className: 'primary button',
                                    onClick: this.toggleReady,
                                    style: {
                                        float : 'right'
                                    }
                                }
                                , 'Ready'
                                )
                            )
                        )
                    );

            } else {
                var wardrobeHref = '/games/' + client.state.gameId + '/wardrobe';

                return React.DOM.div
                    ( { className: 'rmx game-setup-banner' }
                    , React.DOM.i({ className: 'check icon' })
                    , React.DOM.div
                        ( { className: 'content' }
                        , React.DOM.div({ className: 'label' }, 'You need to select a character class and talents')
                        , React.DOM.div
                            ( { className: 'sub' }
                            , Link({ href: wardrobeHref }, 'Go to the wardrobe now.')
                            )
                        )
                    );
            }
        }

        toggleReady() {
            rmx.Game.toggleReady(this.props.client);
        }
    }

    export var GameSetupBanner = createClass(GameSetupBannerSpec);
}
