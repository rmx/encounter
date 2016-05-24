/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Editor/SpellEffect.ts" />
/// <reference path="../../Views/Spell.ts" />
/// <reference path="../../Core/Aura.ts" />


module rmx.Component.View {

    export interface SpellEffectsProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
        resource  : rmx.Storage.Resource<rmx.Storage.Spell>;
    }

    class SpellEffectsSpec extends ReactComponent<SpellEffectsProps, {}> {
        render() {
            var encounter = this.props.encounter
              , resource  = this.props.resource
              , spell     = resource.content;

            var sidebarItems = rmx.Views.spellSidebarItems(encounter, resource.id);

            var spellEffects = spell.effects.map(function(effect) {
                return Editor.SpellEffect({ encounter: encounter, spell: spell, effect: effect, key: effect.id });
            });

            return EncounterResourcePageWithSidebar
                ( { encounter: encounter, resource: resource, sidebarItems: sidebarItems, vertical: true }
                , React.DOM.div
                    ( {}
                    , CreateEffectButtons
                        ( { collection        : spell.effects
                          , effectConstructor : rmx.Storage.SpellEffect
                          , effectTypes       : rmx.Core.spellEffects()
                          }
                        )
                    , spellEffects
                    )
                );
        }
    }

    export var SpellEffects = createClass(SpellEffectsSpec);
}
