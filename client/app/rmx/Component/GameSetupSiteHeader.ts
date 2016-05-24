/// <reference path="./Base.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./Link.ts" />

/// <reference path="../data.ts" />
/// <reference path="../Core/Encounter.ts" />
/// <reference path="../Game/Client.ts" />

module rmx.Component {

    export interface GameSetupSiteHeaderProps {
        client : rmx.Game.Client;
    }

    class GameSetupSiteHeaderSpec extends ReactComponent<GameSetupSiteHeaderProps, {}> {

        render() {
            var client = this.props.client;

            // XXX: Preload the game
            rmx.data.findById<rmx.Storage.Game>(client.gameId).get(null);

            var lobbyUrl    = '/games/' + client.gameId
              , wardrobeUrl = '/games/' + client.gameId + '/wardrobe'
              ;

            return NavBar
                ( {}
                , Link({ href: lobbyUrl }, 'Lobby')
                , navbarVerticalSeparator
                , Link({ href: wardrobeUrl }, 'Wardrobe')
                // , React.DOM.div({ className: 'vertical-separator' })
                // , Link({ href: encounterInfoUrl }, 'Encounter Info')
                , navbarSpacer
                , navbarPeers()
                , React.DOM.div({ className: 'inverted button', onClick: this.exit }, 'Exit')
                );
        }

        exit() {
            var client = this.props.client
              , game   = rmx.data.findById<rmx.Storage.Game>(client.gameId);

            var href = game.fmap(x => {
                if (x.content.purpose === 'grading') {
                    return '/e/' + x.content.encounter.objectId;
                } else {
                    return '/o/' + x.content.encounter.objectId;
                }
            }).get(null);

            if (href) {
                rmx.app.navigateTo(href);
            } else {
                // TODO: ???
            }
        }
    }

    export var GameSetupSiteHeader = createClass(GameSetupSiteHeaderSpec);
}
