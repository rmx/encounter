/// <reference path="./Base.ts" />
/// <reference path="./Drawer.ts" />
/// <reference path="./Link.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    class DrawerLinkItemSpec extends ReactComponent<{ children?: any; href: string }, {}> {

        render() {
            return Link
                ( { href: this.props.href, onClick: this.closeDrawer }
                , this.props.children
                );
        }

        closeDrawer() {
            rmx.data.localState.drawer.open = false;
        }
    }

    export var DrawerLinkItem = createClass(DrawerLinkItemSpec);



    class MainNavSpec extends ReactComponent<{}, {}> {

        render() {
            var admin = rmx.data.findById(<string>rmx.data.session.accountId).fmap(function(account) {
                return React.DOM.section
                    ( {}
                    , DrawerLinkItem({ href: '/accounts' }, 'Accounts')
                    , DrawerLinkItem({ href: '/shards' }, 'Shards')
                    );
            }).get(null);

            return Drawer
                ( {}
                , React.DOM.nav
                    ( { className: 'rmx main-nav' }

                    , React.DOM.section
                        ( {}
                        , DrawerLinkItem({ href: '/' }, 'Dashboard')
                        , DrawerLinkItem({ href: '/help' }, 'Help')
                        // , DrawerLinkItem({ href: '/games' }, 'Games')
                        // , DrawerLinkItem({ href: '/recommendations' }, 'Recommendations')
                        // , DrawerLinkItem({ href: '/browse/new' }, 'New Releases')
                        // , DrawerLinkItem({ href: '/friends' }, 'Friends')
                        )

                    , React.DOM.section
                        ( {}
                        , DrawerLinkItem({ href: '/encounters' }, 'Encounters')
                        , DrawerLinkItem({ href: '/models' }, 'Models')
                        , DrawerLinkItem({ href: '/tiles' }, 'Tiles')
                        , DrawerLinkItem({ href: '/sounds' }, 'Sounds')
                        , DrawerLinkItem({ href: '/icons' }, 'Icons')
                        , DrawerLinkItem({ href: '/particleeffects' }, 'Particle Effects')
                        , DrawerLinkItem({ href: '/skyboxes' }, 'Skyboxes')
                        )

                    , admin
                    )
                );
        }
    }

    export var MainNav = createClass(MainNavSpec);
}
