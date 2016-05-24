/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {


    export interface SpawnPointProps {
        encounter  : rmx.data.Object<rmx.Storage.Encounter>;
        spawnPoint : rmx.Storage.SpawnPoint;
    }

    class SpawnPointSpec extends ReactComponent<SpawnPointProps, {}> {

        render() {
            var encounter = this.props.encounter;

            var terrainOptions = encounter.content.terrainInstances.map(function(x) {
                var value = encounter.objectId + ':terrainInstances.' + x.id;
                return React.DOM.option({ value: value, key: value }, x.id);
            });

            terrainOptions.unshift(React.DOM.option({ value: '', key: '(none)' }, '(none)'));

            var pointOfInterestOptions =
                [ React.DOM.option({ value: '', key: '(none)' }, '(none)')
                ];

            encounter.content.terrainInstances.forEach(ti => {
                var terrainEntityId = encounter.objectId + ':terrainInstances.' + ti.id;
                if (terrainEntityId === this.props.spawnPoint.terrainEntityId) {
                    var terrain = rmx.data.resolveReference<rmx.Storage.Resource<rmx.Storage.Terrain>>(ti.terrain).get(null);
                    if (terrain) {
                        terrain.content.pointsOfInterest.forEach(poi => {
                            pointOfInterestOptions.push
                                ( React.DOM.option({ value: poi.name, key: poi.id }, poi.name)
                                );
                        });
                    }
                }
            });

            return React.DOM.div
                ( { className: 'form-row' }
                , React.DOM.div
                    ( { className: 'form-row-element' }
                    , React.DOM.label({}, 'Terrain Instance')
                    , React.DOM.select
                        ( { value    : this.props.spawnPoint.terrainEntityId
                          , onChange : this.changeTerrainEntityId
                          }
                        , terrainOptions
                        )
                    )
                , React.DOM.div
                    ( { className: 'form-row-element' }
                    , React.DOM.label({}, 'Point of Interest Name')
                    , React.DOM.select
                        ( { value    : this.props.spawnPoint.pointOfInterestName
                          , onChange : this.changePointOfInterestName
                          }
                        , pointOfInterestOptions
                        )
                    )
                );
        }

        changeTerrainEntityId(ev) {
            this.props.spawnPoint.terrainEntityId = ev.target.value;
        }

        changePointOfInterestName(ev) {
            this.props.spawnPoint.pointOfInterestName = ev.target.value;
        }
    }

    export var SpawnPoint = createClass(SpawnPointSpec);
}
