/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../CollectionItemHeader.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game.ts" />


module rmx.Component.View {

    export interface PublicEncounterPageProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    export interface PublicEncounterPageState {
        showHeroNamePrompt ?: boolean;
        heroName           ?: string;
    }

    class PublicEncounterPageSpec extends ReactComponent<PublicEncounterPageProps, PublicEncounterPageState> {

        getInitialState() {
            return { showHeroNamePrompt : false
                   , heroName           : ''
                   };
        }

        render() {
            var encounter          = this.props.encounter
              , backgroundImageUrl = rmx.blobUrl(encounter.content.images.background);

            var style =
                { backgroundImage: 'url(' + backgroundImageUrl + ')'
                };

            return Page
                ( {}
                , React.DOM.div
                    ( { className: "rmx public-encounter-page", style: style }
                    , NavBar
                        ( {}
                        , Link( { href: "/", className: "title" }, 'Home')
                        )
                    , React.DOM.div({ className: 'caption' }, encounter.content.tagline)
                    , this.stuff()
                    )
                );
        }

        stuff() {
            if (this.state.showHeroNamePrompt) {
                return this.heroNamePrompt();
            } else {
                return this.banner();
            }
        }

        startEncounter() {
            if (rmx.data.session.accountId) {
                rmx.createGame
                ( this.props.encounter.objectId
                , rmx.Storage.Purpose.Grading
                , function(err, gameId) {
                    if (err) {
                        alert(err.message);
                    } else {
                        rmx.app.navigateTo('/games/' + gameId);
                    }
                });
            } else {
                this.setState({ showHeroNamePrompt: true });
            }
        }

        banner() {
            var encounter = this.props.encounter;

            return React.DOM.div
                ( { className: 'banner' }
                , React.DOM.h1({ className: 'rmx header' }, encounter.content.name)
                , React.DOM.div
                    ( { className: 'large primary button', onClick: this.startEncounter }
                    , 'Play'
                    )
                );
        }

        heroNamePrompt() {
            return React.DOM.div
                ( { className: 'rmx login' }
                , React.DOM.div
                    ( { className: 'hero-name-prompt form' }
                    , React.DOM.label({}, 'Pick a name for your hero')
                    , React.DOM.input({ className: 'large', value: this.state.heroName, onChange: this.changeHeroName })
                    , React.DOM.button
                        ( { className: 'primary button', onClick: this.startEncounterWithNewHero }
                        , 'Play'
                        )
                    )
                );
        }

        changeHeroName(ev) {
            this.setState({ heroName: ev.target.value });
        }

        startEncounterWithNewHero() {
            var heroName = this.state.heroName;

            rmx.signup(rmx.data.session, heroName, () => {
                if (rmx.data.session.status === rmx.SessionStatus.Authenticated) {
                    this.startEncounter();

                } else {
                    this.setState({});
                }
            });
        }

    }

    export var PublicEncounterPage = createClass(PublicEncounterPageSpec);
}
