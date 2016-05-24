/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../Editor/Expression.ts" />


module rmx.Component.View {

    export interface EncounterScoringFunctionProps {
        encounterId : string;
    }


    class EncounterScoringFunctionSpec extends ReactComponent<EncounterScoringFunctionProps, {}> {
        render() {
            var encounter = rmx.data.findById<rmx.Storage.Encounter>(this.props.encounterId).get(null);
            var expr = encounter.content.scoringFunction.content;

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.DOM.div
                    ( {}
                    , Editor.Expression({ expr: expr, env: 'ScoringFunction' })
                    )
                );
        }
    }

    export var EncounterScoringFunction = createClass(EncounterScoringFunctionSpec);
}
