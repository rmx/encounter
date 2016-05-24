/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../ObjectCardCollection.ts" />


module rmx.Component.View {

    export interface EncounterTerrainsProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterTerrainsSpec extends ReactComponent<EncounterTerrainsProps, {}> {
        render() {
            var encounter = this.props.encounter;


            var terrains = encounter.content.terrainInstances.map(x => {
                function remove() {
                    rmx.data.removeItem(encounter.content.terrainInstances, x);
                }

                var entityId = encounter.objectId + ':terrainInstances.' + x.id;

                return React.DOM.div
                    ( { className: 'rmx encounter-class-reference', key: x.id }
                    , CollectionItemHeader({ id: x.id, text: 'terrain', onRemove: remove })
                    , ReferencePicker({ reference: x.terrain, searchResult: rmx.Core.terrainSearchResult(encounter), toPath: rmx.paths.toResourcePath })
                    , React.DOM.p
                        ( {}
                        , 'This is the entityId of the terrain, use it in scripts '
                        , 'to refer to this terrain: '
                        , React.DOM.strong({}, entityId)
                        )
                    );
            });

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.DOM.div
                    ( {}
                    , React.DOM.div
                        ( {}
                        , React.DOM.div
                            ( { className: 'primary fluid button', onClick: this.createTerrain }
                            , 'Add a new terrain'
                            )
                        )
                    , React.DOM.div({}, terrains)
                    )
                );
        }

        createTerrain() {
            rmx.data.pushItem
                ( this.props.encounter.content.terrainInstances
                , Avers.mk<rmx.Storage.TerrainInstance>(rmx.Storage.TerrainInstance, { terrain: { objectId: '' } })
                );
        }
    }

    export var EncounterTerrains = createClass(EncounterTerrainsSpec);
}
