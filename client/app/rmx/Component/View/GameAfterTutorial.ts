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

    export interface GameAfterTutorialProps {
    }

    class GameAfterTutorialSpec extends ReactComponent<GameAfterTutorialProps, {}> {

        render() {
            return Body
                ( {}
                , Site
                    ( {}
                    , AppNavBar()
                    , React.DOM.main
                        ( { className: 'main' }
                        , React.DOM.div
                            ( { className: 'content game-after',
                                style: { textAlign: 'center', display: 'block' }
                              }
                            , React.DOM.h1
                                ( { style: { flex: '1' } }
                                , 'Thanks for playing the tutorial'
                                )
                            , React.DOM.div
                                ( { style: { flex: '1', marginTop: '2rem' } }
                                , 'The game offers '
                                , React.DOM.i({}, 'Player vs. Creature')
                                , ' (PvC) and two Player vs. Player (PvP): '
                                , React.DOM.i({}, 'Group vs. Group vs. Creatures')
                                , ' (GvGvC) and '
                                , React.DOM.i({}, 'Arena')
                                , ' modes.'
                                )
                            , React.DOM.div
                                ( { style:
                                    { flex: '1'
                                    , display: 'flex'
                                    , flexDirection: 'row'
                                    , marginTop: '4rem'
                                    }
                                  }
                                , React.DOM.div
                                    ( { style: { flex: '1', marginRight: '4rem' } }
                                    , React.DOM.p
                                        ( {}
                                        , 'The GvGvC mode tests your tactical abilities to fight creatures and a hostile '
                                        , 'party at the same time. Only the last party standing (and all objectives '
                                        , 'completed) will receive all the honors. Be warned, restraining your wrath might '
                                        , 'be vital to survive (a certain amount of cooperation with your enemy can be '
                                        , 'essential)!'
                                        )
                                    )
                                , React.DOM.div
                                    ( { style: { flex: '1', marginRight: '4rem' } }
                                    , React.DOM.p
                                        ( {}
                                        , 'In PvC you fight against creatures controlled by an AI. Fight alone or '
                                        , 'choose a group of friends to help you. There are multiple strategies and '
                                        , 'combinations of roles that allow you to win. These short encounters are '
                                        , 'designed to be no longer than 15 minutes.'
                                        )
                                    )
                                , React.DOM.div
                                    ( { style: { flex: '1' } }
                                    , React.DOM.p
                                        ( {}
                                        , 'The <i>Arena</i> mode is the purest form of PvP. Select the perfect combination  '
                                        , 'of roles to beat an unknown group of human players is extremely challenging.  '
                                        , 'Study arena roles, work on team harmony and become champion of the arena!  '
                                        )
                                    )
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

            rmx.createGame
            ( rmx.config.tutorialEncounterId
            , rmx.Storage.Purpose.Grading
            , function(err, gameId) {
                if (err) {
                    alert(err.message);
                } else {
                    rmx.app.navigateTo('/games/' + gameId);
                }
            });
        }

    }

    export var GameAfterTutorial = createClass(GameAfterTutorialSpec);
}
