/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../ObjectCardCollection.ts" />


module rmx.Component.View {

    export interface EncounterSpellsProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterSpellsSpec extends ReactComponent<EncounterSpellsProps, {}> {
        render() {
            var encounter = this.props.encounter
              , resources = encounter.content.resources
              ;

            var cards = resources.filter(res => {
                return res.type === 'spell';
            }).map(function(res) {
                var spell = res.content;

                var spellEffects = spell.effects.map(spellEffect => {
                    switch (Avers.typeName(rmx.Storage.spellEffectTypes, spellEffect.effect.constructor)) {
                    case 'heal':
                        return React.DOM.div
                            ( {}
                            , React.DOM.label({}, 'Heal amount')
                            , NumberInput({ object: spellEffect.effect, field: 'amount' })
                            );
                    case 'spell-damage':
                        return React.DOM.div
                            ( {}
                            , React.DOM.label({}, 'Spell damage amount')
                            , NumberInput({ object: spellEffect.effect, field: 'amount' })
                            );
                    }

                }).filter(x => { return !!x; });

                function onSelect() {
                    rmx.app.navigateTo('/o/' + encounter.objectId + '/resources/' + res.id);
                }

                return React.DOM.div
                    ( { key: res.id }
                    , CollectionItemHeader({ id: res.id, text: spell.name, onSelect: onSelect })
                    , React.DOM.label({}, 'Power Cost')
                    , NumberInput({ object: spell.powerCost, field: 'amount' })
                    , spellEffects
                    );
            });

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.createElement(rmx.Components.EditorNavbar, { encounter: encounter })
                , React.DOM.div({ className: 'form' }, cards)
                );
        }
    }

    export var EncounterSpells = createClass(EncounterSpellsSpec);
}
