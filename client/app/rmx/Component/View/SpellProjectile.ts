/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Optional.ts" />
/// <reference path="../NumberInput.ts" />
/// <reference path="../FormSectionHeader.ts" />
/// <reference path="../../Views/Spell.ts" />


module rmx.Component.View {

    export interface SpellProjectileProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
        resource  : rmx.Storage.Resource<rmx.Storage.Spell>;
    }

    class SpellProjectileSpec extends ReactComponent<SpellProjectileProps, {}> {
        render() {
            var encounter = this.props.encounter
              , resource  = this.props.resource
              , spell     = resource.content;

            var sidebarItems = rmx.Views.spellSidebarItems(encounter, resource.id);

            return EncounterResourcePageWithSidebar
                ( { encounter: encounter, resource: resource, sidebarItems: sidebarItems, vertical: true }
                , React.DOM.div
                    ( {}
                    , FormSectionHeader({ header: 'projectile' })
                    , React.DOM.p
                        ( {}
                        , 'The projectile is used to delay the application of spell effects.'
                        )
                    , Optional
                        ( { opt: spell.projectile }
                        , React.DOM.div
                            ( { className: 'form' }
                            , FormSectionHeader({ header: 'speed' })
                            , NumberInput({ object: spell.projectile, field: 'speed' })
                            )
                        )
                    )
                );
        }
    }

    export var SpellProjectile = createClass(SpellProjectileSpec);
}
