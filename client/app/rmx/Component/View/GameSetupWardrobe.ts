/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../GameSetupBanner.ts" />
/// <reference path="../GameSetupSiteHeader.ts" />
/// <reference path="../GameSetupBody.ts" />
/// <reference path="../GameSetupChat.ts" />
/// <reference path="../GameSetupWardrobeContent.ts" />
/// <reference path="../RequestAnimationFrameMixin.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Core/Encounter.ts" />
/// <reference path="../../Game/Client.ts" />

module rmx.Component.View {

    export interface GameSetupWardrobeProps {
        client : rmx.Game.Client;
    }

    class GameSetupWardrobeSpec extends ReactComponent<GameSetupWardrobeProps, {}> {

        render() {
            var client = this.props.client;

            if (client.state.stage === rmx.Pure.Stage.Running) {
                rmx.app.navigateTo('/games/' + client.gameId);
            }

            return GameSetupBody
                ( { client: client }
                , GameSetupBanner({ client: client })
                , React.DOM.div
                    ( { className: 'rmx game-setup-content' }
                    , GameSetupWardrobeContent({ client: client })
                    , React.DOM.div
                        ( { className: 'rmx game-setup-sidebar' }
                        , GameSetupChat({ client: client })
                        )
                    )
                );
        }
    }

    /// XXX: Should not be needed
    GameSetupWardrobeSpec.prototype.mixins =
        [ // RequestAnimationFrameMixin
        ];


    export var GameSetupWardrobe = createClass(GameSetupWardrobeSpec);
}
