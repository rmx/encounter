module rmx.Views {

    export interface ISidebarItem {
        label: string;
        description: string;
        iconClass: string;
        href: string;
    }


    // Settings
    // -----------------------------------------------------------------------

    export function settingsSidebarItems(): ISidebarItem[] {
        var contextPath = '/settings';

        return [ { href: contextPath + '/profile'
                 , iconClass: 'icon-cog'
                 , label: 'Profile'
                 , description: 'Your profile'
                 }
               , { href: contextPath + '/friends'
                 , iconClass: 'icon-cog'
                 , label: 'Friends'
                 , description: 'Your friends'
                 }
               , { href: contextPath + '/graphics'
                 , iconClass: 'icon-cog'
                 , label: 'Graphics'
                 , description: 'Graphics settings'
                 }
               , { href: contextPath + '/audio'
                 , iconClass: 'icon-cog'
                 , label: 'Audio'
                 , description: 'Audio settings'
                 }
               ];
    }

    // Collections
    // -----------------------------------------------------------------------

    export function
    collectionView
    ( collection      : rmx.data.Collection
    , type            : string
    , contentTemplate : any
    , toPublicPage   ?: boolean
    ): View {
        return new View(() => {
            return React.createElement(rmx.Components.Collection,
                { type         : type
                , collection   : collection
                , template     : Avers.toJSON(contentTemplate)
                , toPublicPage : toPublicPage
                }
            );
        });
    }



    // Misc
    // -----------------------------------------------------------------------

    export function
    notFoundView(info: string = 'Not Found'): View {
        return new View(() => {
            return rmx.Component.View.NotFound({ info: info });
        });
    }
}
