/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />
/// <reference path="../../Core/Validation.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />


module rmx.Component.View {

    export interface EncounterIssuesProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterIssuesSpec extends ReactComponent<EncounterIssuesProps, {}> {
        render() {
            var encounter = this.props.encounter;

            var val = rmx.Core.validateEncounter(encounter);

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.DOM.div
                    ( {}
                    , React.DOM.h2({}, val.issues.length, ' issues detected in this encounter')
                    , val.issues.map(issue => { return renderIssue(issue); })
                    )
                );
        }
    }

    export var EncounterIssues = createClass(EncounterIssuesSpec);


    function
    renderIssue(issue: rmx.Core.Issue) {
        return Link
            ( { href: issue.path.toPath(), key: issue.title }
            , React.DOM.div({}, issue.title)
            );
    }
}
