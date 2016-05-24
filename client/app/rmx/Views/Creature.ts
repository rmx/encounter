module rmx.Views {

    export function
    creatureSidebarItems
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , assetId   : string
    ): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId + '/resources/' + assetId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Settings'
                 , description:  '...'
                 }
               , { href:         contextPath + '/spells'
                 , iconClass:    'icon-cog'
                 , label:        'Spells'
                 , description:  'Define the spells which this creature can use in the encounter.'
                 }
               ];
    }

    export function
    creatureMainView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Creature>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.CreatureMainView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    creatureSpellsView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Creature>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.CreatureSpellsView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }
}
