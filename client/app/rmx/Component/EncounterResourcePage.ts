/// <reference path="../data.ts" />
/// <reference path="../Game.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Sidebar.ts" />
/// <reference path="./Page.ts" />
/// <reference path="./MainNav.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./DrawerCloser.ts" />
/// <reference path="./Main.ts" />
/// <reference path="./Link.ts" />


module rmx.Component {

    export interface EncounterResourcePageProps {
        encounter;
        resource;
        children?;
        vertical?;
    }

    class EncounterResourcePageSpec extends ReactComponent<EncounterResourcePageProps, {}> {
        render() {
            var encounter     = this.props.encounter
              , resource      = this.props.resource
              , mainDirection = this.props.vertical ? 'vertical' : '';

            var encounterHref = '/o/' + encounter.objectId + '/resources';

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: encounterHref }, encounter.content.name)
                    , React.DOM.div({ className: 'resource' }, resource.content.name)
                    , navbarSpacer
                    , React.DOM.div
                        ( { className: 'rmx link title', onClick: this.startEncounter }
                        , React.DOM.i({ className: 'play sign icon' })
                        , 'Play'
                        )
                    )
                , React.DOM.div
                    ( { className: 'rmx page-with-sidebar' }
                    , Main({ className: mainDirection }, this.props.children)
                    )
                );
        }

        startEncounter() {
            rmx.createGame
            ( this.props.encounter.objectId
            , rmx.Storage.Purpose.Verification
            , function(err, gameId) {
                if (err) {
                    alert(err.message);
                } else {
                    rmx.app.navigateTo('/games/' + gameId);
                }
            });
        }
    }

    export var EncounterResourcePage = createClass(EncounterResourcePageSpec);
}
