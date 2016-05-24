module rmx.Views.Class {

    export function
    sidebarItems(encounter: rmx.data.Object<rmx.Storage.Encounter>, assetId: string): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId + '/resources/' + assetId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Settings'
                 , description:  'Basic class settings.'
                 }
               , { href:         contextPath + '/spells'
                 , iconClass:    'icon-cog'
                 , label:        'Spells'
                 , description:  '...'
                 }
               ];
    }

    export function
    mainView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , assetId   : string
    , resource  : rmx.Storage.Resource<rmx.Storage.Class>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.ClassView, { encounter: enc, sidebarItems : sidebarItems(encounter, assetId), resource: resource });
        }

        return new rmx.View(body);
    }
}
