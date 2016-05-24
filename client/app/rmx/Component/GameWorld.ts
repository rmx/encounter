/// <reference path="./Base.ts" />
/// <reference path="./Chrome/Parent.ts" />
/// <reference path="../Game.ts" />

module rmx.Component {

    export interface GameWorldProps {
        client : rmx.Game.Client;
    }

    class GameWorldSpec extends ReactComponent<GameWorldProps, {}> {
        render() {
            var client = this.props.client;

            return Body
                ( {}
                , Site
                    ( {}
                    , GameHeader({ client: client })
                    , React.DOM.div
                        ( { className: 'rmx game-view' }
                        , GameCanvas({ client: client })
                        , Chrome.Parent({ client: client })
                        )
                    )
                );
        }
    }

    export var GameWorld = createClass(GameWorldSpec);
}
