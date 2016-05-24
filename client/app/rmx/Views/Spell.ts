/// <reference path="../../entry.ts" />
/// <reference path="../Views.ts" />

module rmx.Views {

    export function
    spellSidebarItems
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resourceId : string
    ): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId + '/resources/' + resourceId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Settings'
                 , description:  'Basic spell settings.'
                 }
               , { href:         contextPath + '/projectile'
                 , iconClass:    'icon-layers'
                 , label:        'Projectile'
                 , description:  'Definition of the optional projectile.'
                 }
               , { href:         contextPath + '/effects'
                 , iconClass:    'icon-layers'
                 , label:        'Effects'
                 , description:  'The in-game effects that the spell has on its targets.'
                 }
               , { href:         contextPath + '/visual'
                 , iconClass:    'icon-monitor-screen'
                 , label:        'Appearance and sound'
                 , description:  'Particle effects, model animations, sounds.'
                 }
               ];
    }

    export function
    spellMainView
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resource   : rmx.Storage.Resource<rmx.Storage.Spell>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.SpellMainView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    spellVisualView
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resource   : rmx.Storage.Resource<rmx.Storage.Spell>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.SpellVisualView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }
}
