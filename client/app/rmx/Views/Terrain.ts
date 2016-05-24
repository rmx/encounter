/// <reference path="../Editor/TerrainEditor.ts" />

module rmx.Views {

    export function
    terrainSidebarItems
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resourceId : string
    ): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId + '/resources/' + resourceId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Settings'
                 , description:  'Basic terrain settings.'
                 }
               , { href:         contextPath + '/layout'
                 , iconClass:    'icon-cog'
                 , label:        'Layout'
                 , description:  '...'
                 }
               ];
    }

    export function
    terrainMainView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Terrain>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.TerrainResource,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    terrainLayoutView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Terrain>
    ): rmx.View {
        var terrainEditor = new rmx.Editor.TerrainEditor(resource.content);

        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.TerrainLayoutView,
                { encounter     : enc
                , resource      : resource
                , terrainEditor : terrainEditor
                }
            );
        }

        return new rmx.View(body);
    }
}
