/// <reference path="./Base.ts" />
/// <reference path="./Body.ts" />
/// <reference path="./Site.ts" />
/// <reference path="./Exception.ts" />
/// <reference path="./MainNav.ts" />
/// <reference path="./DrawerCloser.ts" />
/// <reference path="./GameSetupSiteHeader.ts" />
/// <reference path="./RequestAnimationFrameMixin.ts" />
/// <reference path="./LoadingScreen.ts" />

/// <reference path="../data.ts" />
/// <reference path="../Game/Client.ts" />

module rmx.Component {

    export interface GameSetupBodyProps extends ReactProps {
        client : rmx.Game.Client;
    }

    class GameSetupBodySpec extends ReactComponent<GameSetupBodyProps, {}> {
        render() {
            var client = this.props.client;

            if (client.error) {
                return Body
                    ( {}
                    , Site
                        ( {}
                        , MainNav()
                        , DrawerCloser()

                        , GameSetupSiteHeader({ client: client })
                        , Exception({ error: client.error })
                        )
                    );

            } else {
                // Show a loading screen in the main area until the encounter is loaded,
                // so we have a uniform loading behavior across all game setup bodies.
                var encounterId = client.state.encounterId;
                var children    = rmx.data.findById(encounterId).fmap(entity => {
                    return this.props.children;
                }).get(LoadingScreen());

                return Body
                    ( {}
                    , Site
                        ( {}
                        , MainNav()
                        , DrawerCloser()

                        , GameSetupSiteHeader({ client: client })

                        , React.DOM.main
                            ( { className: 'rmx game-setup-main' }
                            , children
                            )
                        )
                    );
            }
        }
    }

    // FIXME: Remove rAF, let the game client/state object increment
    // the generation number when it changes (in response to incoming packets).
    GameSetupBodySpec.prototype.mixins =
        [ // RequestAnimationFrameMixin
        ];


    export var GameSetupBody = createClass(GameSetupBodySpec);
}
