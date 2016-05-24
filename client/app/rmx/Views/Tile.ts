module rmx.Views.Tile {

    function sidebarItems(contextPath: string): ISidebarItem[] {
        return [ { href: contextPath + '/'
                 , iconClass: 'icon-cog'
                 , label: 'Preview'
                 , description: 'Preview the tile in all its glory.'
                 },

                 { href: contextPath + '/model'
                 , iconClass: 'icon-cog'
                 , label: 'Model'
                 , description: 'The model that is rendered in place of this tile.'
                 },

                 { href: contextPath + '/surface'
                 , iconClass: 'icon-cog'
                 , label: 'Surface'
                 , description: 'Define the surface on which characters can walk.'
                 },
               ];
    }

    function headerTitle(tile) {
        return Views.objectHeaderTitle(tile, '/tiles', 'All tiles');
    }

    export function
    modelHeader(parentPath: string, parent: rmx.Storage.Tile): IHeaderTitle {
        return { contextHref  : parentPath
               , contextLabel : parent.name
               , mainTitle    : 'Model'
               };
    }


    export function mainView(contextPath: string, tile: rmx.Storage.Tile): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(tile) })
                , React.createElement(rmx.Components.TileMainView, { sidebarItems : sidebarItems(contextPath), tile: tile })
                );
        }

        return new rmx.View(body);
    }

    export function surfaceView(contextPath: string, tile: rmx.Storage.Tile): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(tile) })
                , React.createElement(rmx.Components.TileSurfaceView, { sidebarItems : sidebarItems(contextPath), tile: tile })
                );
        }

        return new rmx.View(body);
    }
}
