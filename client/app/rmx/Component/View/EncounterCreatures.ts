/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../ObjectCardCollection.ts" />


module rmx.Component.View {

    export interface EncounterCreaturesProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterCreaturesSpec extends ReactComponent<EncounterCreaturesProps, {}> {
        render() {
            var encounter = this.props.encounter
              , resources = encounter.content.resources
              ;

            var cards = resources.filter(res => {

                return res.type === 'creature' || res.type === 'class';
            }).map(function(res) {

                var name = res.content.name + ' (' + res.type +')';

                var health = null;
                if (res.type === 'creature') {
                    health = res.content.health;
                } else if (res.type === 'class') {
                    health = res.content.creature.health;
                }

                function onSelect() {
                    rmx.app.navigateTo('/o/' + encounter.objectId + '/resources/' + res.id);
                }

                return React.DOM.div
                    ( { key: res.id }
                    , CollectionItemHeader({ id: res.id, text: name, onSelect: onSelect })
                    , React.DOM.label({}, 'health')
                    , React.DOM.div
                        ( { className: 'form-row' }
                        , React.DOM.div
                            ( { className: 'form-row-element' }
                            , React.DOM.label({}, 'min')
                            , NumberInput({ object: health, field: 'min' })
                            )
                        , React.DOM.div
                            ( { className: 'form-row-element' }
                            , React.DOM.label({}, 'max')
                            , NumberInput({ object: health, field: 'max' })
                            )
                        )
                    );
            });

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.createElement(rmx.Components.EditorNavbar, { encounter: encounter })
                , React.DOM.div({ className: 'form' }, cards)
                );
        }
    }

    export var EncounterCreatures = createClass(EncounterCreaturesSpec);
}
