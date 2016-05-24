/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../GameSetupBanner.ts" />
/// <reference path="../GameSetupChat.ts" />
/// <reference path="../GameSetupSiteHeader.ts" />
/// <reference path="../GameSetupBody.ts" />
/// <reference path="../RequestAnimationFrameMixin.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Core/Encounter.ts" />
/// <reference path="../../Core/Game.ts" />
/// <reference path="../../Game/Client.ts" />

declare var classNames;

module rmx.Component.View {

    export interface GameLobbyProps {
        client : rmx.Game.Client;
    }

    class GameLobbySpec extends ReactComponent<GameLobbyProps, {}> {

        render() {
            var client      = this.props.client
              , encounterId = client.state.encounterId;

            var children = rmx.data.objectContent<rmx.Storage.Encounter>(encounterId).fmap(encounter => {
                var parties = encounter.parties.map(party => {
                    return lobbyParty(client, party);
                });

                return React.DOM.div
                    ( { className: 'rmx game-setup-wardrobe-content vertical' }
                    , parties
                    );
            }).get(<any>LoadingScreen());

            return GameSetupBody
                ( { client: client }
                , GameSetupBanner({ client: client })
                , React.DOM.div
                    ( { className: 'rmx game-setup-content' }
                    , React.DOM.div
                        ( { className: 'rmx game-setup-lobby' }
                        , children
                        )
                    , React.DOM.div
                        ( { className: 'rmx game-setup-sidebar' }
                        , GameSetupChat({ client: client })
                        )
                    )
                );
        }
    }

    export var GameLobby = createClass(GameLobbySpec);



    function lobbyParty(client: rmx.Game.Client, storageParty: rmx.Storage.Party): React.ReactNode {
        // FIXME: lookupParty may return null
        var party = rmx.Pure.lookupParty(client.state, storageParty.id);

        // FIXME: playerParty may return null
        var currentParty = rmx.Pure.playerParty(client.state, rmx.data.session.accountId);

        function changeParty() {
            rmx.Game.changeParty(client, storageParty.id);
        }

        var className = "";
        if (currentParty.id === storageParty.id) {
            className = "selected-party";
        }

        var partyMembers = party.players.map(player => {
            return WardrobePlayer({ client: client, player: player, key: player.accountId });
        });

        if (client.state.parties.length === 1) {
            return partyMembers;
        } else {
            return React.DOM.div
                ( { className: className, key: party.id }
                , React.DOM.h2({}, 'Party ', party.id)
                , React.DOM.div
                    ( { className: 'small primary button', onClick: changeParty }
                    , 'Select party'
                    )
                , React.DOM.div
                    ( { className: 'rmx party-players' }
                    , partyMembers
                    )
                );
        }
    }



    export interface WardrobePlayerProps {
        client : rmx.Game.Client;
        player : rmx.Pure.Player;
    }

    class WardrobePlayerSpec extends ReactComponent<WardrobePlayerProps, {}> {

        render() {
            var player         = this.props.player
              , portraitUrl    = rmx.Core.accountAvatarUrl(player.accountId)
              , playerClass    = rmx.Core.playerClassName(player).get('Undecided')
              , className      = player.roleId ? 'class' : 'undecided class'
              , nick           = rmx.Core.accountLogin(player.accountId)
              ;

            return React.DOM.div
                ( { className: this.elementClass() }
                , React.DOM.img({ src: portraitUrl })
                , React.DOM.div
                    ( { className: 'detail' }
                    , React.DOM.div({ className: 'nick' }, nick)
                    , React.DOM.div({ className: className }, playerClass)
                    )
                );
        }

        elementClass() {
            return classNames(
                { rmx                     : 1
                , 'wardrobe-party-member' : 1
                , ready                   : this.props.player.ready
                }
            );
        }
    }

    export var WardrobePlayer = createClass(WardrobePlayerSpec);
}
