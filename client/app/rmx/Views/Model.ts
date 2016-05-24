module rmx.Views.Model {

    function sidebarItems(contextPath: string): ISidebarItem[] {
        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Preview'
                 , description:  'Preview the model in all its glory.'
                 }

               , { href:         contextPath + '/geometry'
                 , iconClass:    'icon-network'
                 , label:        'Geometry'
                 , description:  'Change the model geometry.'
                 }

               , { href:         contextPath + '/skins'
                 , iconClass:    'icon-palette'
                 , label:        'Skins'
                 , description:  'All skins used by the model.'
                 }

               , { href:         contextPath + '/animations'
                 , iconClass:    'icon-palette'
                 , label:        'Animations'
                 , description:  'Animations...'
                 }
               ];
    }


    export function mainView(contextPath: string, model: rmx.Storage.Model, header: IHeaderTitle): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: header })
                , React.createElement(rmx.Components.ModelMainView, { sidebarItems: sidebarItems(contextPath), model: model })
                );
        }

        return new rmx.View(body);
    }

    export function geometryView(contextPath: string, model: rmx.Storage.Model, header: IHeaderTitle): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: header })
                , React.createElement(rmx.Components.ModelGeometryView, { sidebarItems: sidebarItems(contextPath), model: model })
                );
        }

        return new rmx.View(body);
    }

    export function skinsView(contextPath: string, model: rmx.Storage.Model, header: IHeaderTitle): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: header })
                , React.createElement(rmx.Components.ModelSkinsView, { sidebarItems: sidebarItems(contextPath), model: model })
                );
        }

        return new rmx.View(body);
    }

    export function animationsView(contextPath: string, model: rmx.Storage.Model, header: IHeaderTitle): rmx.View {
        function body() {
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: header })
                , React.createElement(rmx.Components.ModelAnimationsView, { sidebarItems: sidebarItems(contextPath), model: model })
                );
        }

        return new rmx.View(body);
    }
}
