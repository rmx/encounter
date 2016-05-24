/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../MainNav.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../DrawerCloser.ts" />
/// <reference path="../GameSetupSiteHeader.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Core/Game.ts" />


module rmx.Component.View {

    export interface GameAfterProps {
        gameId : string;
    }

    class GameAfterSpec extends ReactComponent<GameAfterProps, {}> {

        render() {
            var leaderboard = [];
            var gameName = "Thanks for playing ";

            var gameC = rmx.data.findById<rmx.Storage.Game>(this.props.gameId);
            var encounterC = gameC.bind(game =>
                rmx.data.findById<rmx.Storage.Encounter>(game.content.encounter.objectId));

            Computation.liftA2<rmx.data.Object<rmx.Storage.Game>, rmx.data.Object<rmx.Storage.Encounter>, any>(
                gameC, encounterC, (game, encounter) => {

                var encounterId = encounter.objectId;
                gameName += encounter.content.name;

                leaderboard =
                    rmx.data.encounterLeaderboard.get(encounterId).ids.fmap(ids => {
                    return ids.map((id, rank) => {
                        return rmx.data.findById<rmx.Storage.Game>(id).fmap(game => {
                            var party = rmx.Core.winningParty(game.content);
                            var accountNames = party.players.map(player => {
                                return Link
                                    ( { href: '/o/' + player.id }
                                    , rmx.Core.accountLogin(player.id)
                                    );
                            });

                            return React.DOM.tr
                                ( {}
                                , React.DOM.td({}, rank + 1)
                                , React.DOM.td({}, accountNames)
                                , React.DOM.td({}, game.content.duration.toFixed(2))
                                , React.DOM.td({}, party.score)
                                , React.DOM.td({}, Time({ datetime: game.createdAt.toISOString() }))
                                );
                        }).get(null);
                    }).filter(x => { return !!x; });
                }).get([]);
            }).get(null);

            return Body
                ( {}
                , Site
                    ( {}
                    , AppNavBar()
                    , React.DOM.main
                        ( { className: 'main' }
                        , React.DOM.div
                            ( { className: 'content game-after',
                                style: {
                                paddingTop: '50px',
                                display: 'flex', flexDirection: 'column',
                                justifyContent: 'center',
                                alignItems: 'center',
                                alignContent: 'center' } }
                            , React.DOM.h1
                                ( { style: { flex: '1' } }
                                , gameName
                                )
                            , React.DOM.table
                                ( {style: { flex : '1' }}
                                , React.DOM.thead
                                    ( {}
                                    , React.DOM.tr
                                        ( {}
                                        , React.DOM.th({} , 'Rank')
                                        , React.DOM.th({} , 'Players')
                                        , React.DOM.th({} , 'Duration')
                                        , React.DOM.th({} , 'Score')
                                        , React.DOM.th({} , 'Date')
                                        )
                                    )
                                , leaderboard
                                )
                            , React.DOM.div
                                ( { style: {
                                    flex: '1'
                                  , paddingTop: "50px"
                                  , flexDirection: 'row' }}
                                , Link({className: 'primary button'
                                       , style: {marginRight: '30px'}
                                       , href: '/' }
                                       , "Choose Encounter")
                                , Link({className: 'primary button'
                                       , style: {marginRight: '30px'}
                                       , href: '/openGames' }
                                       , "Open Encounters")
                                , React.DOM.div
                                    ( { className: 'button'
                                      , onClick: this.startEncounter
                                      }
                                    , "Retry Encounter"
                                    )
                                )
                            )
                        )
                    )
                );
        }

        startEncounter() {
            // XXX: We assume that the user is logged in. Anonymous users
            // should never see this view.

            // XXX: We assume the game is already loaded in the client,
            // otherwise we'd have to use a promise.

            rmx.data.objectContent<rmx.Storage.Game>(this.props.gameId).fmap(game => {
                rmx.createGame
                ( game.encounter.objectId
                , rmx.Pure.JSON.enumFromJSON(rmx.Storage.Purpose, game.purpose)
                , function(err, gameId) {
                    if (err) {
                        alert(err.message);
                    } else {
                        rmx.app.navigateTo('/games/' + gameId);
                    }
                });
            }).get(null);
        }

    }

    export var GameAfter = createClass(GameAfterSpec);
}
