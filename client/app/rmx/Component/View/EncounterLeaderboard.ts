/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../CollectionItemHeader.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />


module rmx.Component.View {

    export interface EncounterLeaderboardProps {
        encounterId : string;
    }


    function winningParty(game: rmx.Storage.Game): rmx.Storage.GameParty {
        var ret = null;

        game.parties.forEach(party => {
            if (party.objectives.every(x => { return x.isCompleted; })) {
                if (ret) {
                    if (party.score > ret.score) {
                        ret = party;
                    }
                } else {
                    ret = party;
                }
            }
        });

        return ret;
    }


    class EncounterLeaderboardSpec extends ReactComponent<EncounterLeaderboardProps, {}> {
        render() {
            var encounterId = this.props.encounterId;

            var leaderboard = rmx.data.encounterLeaderboard.get(encounterId).ids.fmap(ids => {
                return ids.map(id => {
                    return rmx.data.findById<rmx.Storage.Game>(id).fmap(game => {
                        var party = winningParty(game.content);
                        var accountNames = party.players.map(player => {
                            return rmx.data.findById<rmx.Storage.Account>(player.id).fmap(account => {
                                return Link({ href: '/o/' + player.id }, account.content.login);
                            }).get(null);
                        });

                        return React.DOM.div
                            ( { className: 'rmx encounter-leaderboard-entry' }
                            , CollectionItemHeader({ id: id, text: id })
                            , 'duration: '
                            , game.content.duration
                            , ', score: '
                            , party.score
                            , ', players: '
                            , accountNames
                            );
                    }).get(null);
                }).filter(x => { return !!x; });
            }).get([]);

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: '/e/' + encounterId }, encounterId)
                    )
                , Main
                    ( {}
                    , React.DOM.div
                        ( { style: { flex: 1 } }
                        , leaderboard
                        )
                    )
                );
        }
    }

    export var EncounterLeaderboard = createClass(EncounterLeaderboardSpec);
}
