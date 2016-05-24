/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../ObjectCardCollection.ts" />


module rmx.Component.View {

    export interface EncounterResourcesProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterResourcesSpec extends ReactComponent<EncounterResourcesProps, {}> {
        render() {
            var encounter    = this.props.encounter
              , resources    = encounter.content.resources
              , filterString = rmx.data.localState.resourceFilter
              , filter       = filterString ? new RegExp(filterString, 'i') : /.*/;

            var cards = resources.filter(res => {

                return (res.content && (filter.test(res.content.name) || filter.test(res.content.type))) || filter.test(res.type);
            }).map(function(res) {
                return React.createElement(rmx.Components.ResourceCard, { encounter: encounter, resource: res, key: res.id });
            });

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.createElement(rmx.Components.EditorNavbar, { encounter: encounter })
                , ObjectCardCollection({}, cards)
                );
        }
    }

    export var EncounterResources = createClass(EncounterResourcesSpec);
}
