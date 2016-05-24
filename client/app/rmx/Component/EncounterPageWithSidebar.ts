/// <reference path="../data.ts" />
/// <reference path="../Game.ts" />
/// <reference path="../Views/Encounter.ts" />
/// <reference path="../Core/Validation.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Sidebar.ts" />
/// <reference path="./Page.ts" />
/// <reference path="./MainNav.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./DrawerCloser.ts" />
/// <reference path="./Main.ts" />
/// <reference path="./Link.ts" />


module rmx.Component {

    export interface EncounterPageWithSidebarProps extends ReactProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
        vertical  : boolean;
    }

    class EncounterPageWithSidebarSpec extends ReactComponent<EncounterPageWithSidebarProps, {}> {
        render() {
            var encounter     = this.props.encounter
              , sidebarItems  = rmx.Views.Encounter.mkSidebarItems(encounter)
              , mainDirection = this.props.vertical ? 'vertical' : '';

            var encounterHref = '/o/' + encounter.objectId + '/resources';

            var issuesLink = null;

            var val = rmx.Core.validateEncounter(encounter);
            if (val.issues.length > 0) {
                issuesLink = Link
                    ( { className: 'rmx link title', href: '/o/' + encounter.objectId + '/issues' }
                    , React.DOM.i({ className: 'warning sign icon' })
                    , val.issues.length
                    , ' issues'
                    );
            }


            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: encounterHref }, encounter.content.name)
                    , navbarSpacer
                    , navbarPeers()
                    , navbarVerticalSeparator
                    , issuesLink
                    , React.DOM.div
                        ( { className: 'rmx link title', onClick: this.startEncounter }
                        , React.DOM.i({ className: 'play sign icon' })
                        , 'Play'
                        )
                    )
                , React.DOM.div
                    ( { className: 'rmx page-with-sidebar' }
                    , Sidebar({ items: sidebarItems })
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

    export var EncounterPageWithSidebar = createClass(EncounterPageWithSidebarSpec);
}
