/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../RequestAnimationFrameMixin.ts" />
/// <reference path="../MainNav.ts" />
/// <reference path="../DrawerCloser.ts" />
/// <reference path="../LoadingScreen.ts" />
/// <reference path="../GameHeader.ts" />

/// <reference path="../Chrome/ConsoleMessage.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />


module rmx.Component.View {

    export interface GameLogProps {
        gameId : string;
    }

    class GameLogSpec extends ReactComponent<GameLogProps, {}> {

        render() {
            var client = rmx.openGame(this.props.gameId);

            var children = client.state.consoleMessages.map((msg, index) => {
                return rmx.Component.Chrome.ConsoleMessage({ client: client, msg: msg, key: index });
            });

            return Body
                ( {}
                , Site
                    ( {}
                    , GameHeader({ client: client })
                    , React.DOM.main
                        ( { className: 'main' }
                        , React.DOM.div
                            ( {}
                            , children
                            )
                        )
                    )
                );
        }
    }

    export var GameLog = createClass(GameLogSpec);
}
