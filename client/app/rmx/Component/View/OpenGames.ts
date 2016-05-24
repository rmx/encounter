/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../Time.ts" />
/// <reference path="../LoadingScreen.ts" />
/// <reference path="../ObjectCardCollection.ts" />
/// <reference path="../../Components.ts" />


module rmx.Component.View {

    function backgroundImageStyle(blobId) {
        if (blobId) {
            return { backgroundImage: 'url(' + rmx.blobUrl(blobId) + ')' };
        } else {
            return {};
        }
    }

    function openGame(gameC) {
        var encounterC = gameC.bind(game => {
            return rmx.data.objectContent(game.content.encounter.objectId);
        });

        return Computation.liftA2<rmx.data.Object<rmx.Storage.Game>, rmx.Storage.Encounter, any>(gameC, encounterC, (game, encounter) => {
            var href = '/games/' + game.objectId;

            var style = backgroundImageStyle(encounter.images.featureSquare);

            return Link
                ( { className: 'rmx open-game', href: href }
                , React.DOM.div({ className: 'image', style: style })
                , React.DOM.div
                    ( { className: 'details' }
                    , React.DOM.div({ className: 'title' }, encounter.name)
                    , React.DOM.div({ className: 'tagline' }, encounter.tagline)
                    , React.DOM.div({}, 'Created ', Time({ datetime: game.createdAt.toUTCString() }))
                    )
                );

        }).get(null);
    }

    class OpenGamesSpec extends ReactComponent<{}, {}> {

        render() {
            var loadingScreen = Page
                ( {}
                , AppNavBar()
                , Main
                    ( { style: { flexDirection: 'column' } }
                    , LoadingScreen()
                    )
                );

            return rmx.data.openGames.ids.fmap(ids => {
                if (ids.length === 0) {
                    return Page
                        ( {}
                        , AppNavBar()
                        , React.DOM.div
                            ( { style: {display:'flex',flex:1,justifyContent:'center',alignItems:'center'} }
                            , React.DOM.div
                                ( { style: { width: '20rem' } }
                                , React.DOM.p({}, 'There are currently no open games. You\'ll have to pick an encounter and start a new game.')
                                , React.DOM.a({ className: 'fluid primary button', href: '/' }, 'browse encounters')
                                )
                            )
                        );

                } else {
                    var games = ids.filter(id => {

                        // TODO: Implement filter
                        return true;

                    }).map(id => {
                        var game = rmx.data.findById(id);
                        return openGame(game);
                    });

                    return Page
                        ( {}
                        , AppNavBar()
                        , Main
                            ( { style: { flexDirection: 'column' } }
                            , React.DOM.div
                                ( {}
                                , games
                                )
                            )
                        );
                }

            }).get(loadingScreen);
        }
    }

    export var OpenGames = createClass(OpenGamesSpec);
}
