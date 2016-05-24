/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Input.ts" />
/// <reference path="../Editor/Script.ts" />

/// <reference path="../EncounterResourcePage.ts" />


module rmx.Component.View {

    export interface BehaviorResourceProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
        resource  : rmx.Storage.Resource<rmx.Storage.Behavior>;
    }

    class BehaviorResourceSpec extends ReactComponent<BehaviorResourceProps, {}> {
        render() {
            var encounter = this.props.encounter
              , resource  = this.props.resource
              , behavior  = resource.content;

            return EncounterResourcePage
                ( { encounter: encounter, resource: resource }
                , React.DOM.div
                    ( { className: 'vertical form' }

                    , React.DOM.label({}, 'name')
                    , Input({ object: behavior, field: 'name' })

                    , React.DOM.label({}, 'script')
                    , Editor.Script({ object: behavior, field: 'script' })
                    )
                );
        }
    }

    export var BehaviorResource = createClass(BehaviorResourceSpec);
}
