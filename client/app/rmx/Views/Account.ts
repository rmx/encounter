module rmx.Views.Account {

    function sidebarItems(account: rmx.data.Object<rmx.Storage.Account>): ISidebarItem[] {
        var contextPath = '/o/' + account.objectId;

        return [ { href:         contextPath
                 , iconClass:    'icon-id-card'
                 , label:        'Profile'
                 , description:  'The account profile.'
                 }
               , { href:         contextPath + '/friends'
                 , iconClass:    'icon-id-card'
                 , label:        'Friends'
                 , description:  'Your friends.'
                 }
               ];
    }

    function headerTitle(account) {
        return Views.objectHeaderTitle(account, '/accounts', 'All accounts');
    }

    export function mainView(objectId: string): rmx.View {
        function body() {
            var account =
                rmx.data.findById<rmx.Storage.Account>(objectId).get(null);
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(account.content) })
                , React.createElement(rmx.Components.Account, { sidebarItems: sidebarItems(account), account: account })
                );
        }

        return new rmx.View(body);
    }

    export function friendsView(objectId: string): rmx.View {
        function body() {
            var account =
                rmx.data.findById<rmx.Storage.Account>(objectId).get(null);
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(account.content) })
                , React.createElement(rmx.Components.AccountFriends, { sidebarItems: sidebarItems(account), account: account })
                );
        }

        return new rmx.View(body);
    }

    export function bindingsView(objectId: string): rmx.View {
        function body() {
            var account =
                rmx.data.findById<rmx.Storage.Account>(objectId).get(null);
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(account.content) })
                , React.createElement(rmx.Components.AccountBindings, { sidebarItems: sidebarItems(account), account: account })
                );
        }

        return new rmx.View(body);
    }
}

module rmx.Views {
    export function
    objectHeaderTitle(obj: any, contextHref: string, contextLabel: string): IHeaderTitle {
        return { mainTitle    : obj.name
               , contextHref  : contextHref
               , contextLabel : contextLabel
               };
    }
}
