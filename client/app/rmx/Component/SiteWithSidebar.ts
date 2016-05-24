/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Sidebar.ts" />
/// <reference path="./Site.ts" />
/// <reference path="./MainNav.ts" />
/// <reference path="./DrawerCloser.ts" />
/// <reference path="./Main.ts" />


module rmx.Component {

    export interface SiteWithSidebarProps {
        sidebarItems;
        children;
    }

    class SiteWithSidebarSpec extends ReactComponent<SiteWithSidebarProps, {}> {
        render() {
            return Site
                ( {}
                , MainNav()
                , DrawerCloser()
                , Main
                    ( {}
                    , Sidebar({ items: this.props.sidebarItems })
                    , React.DOM.div({ className: 'content' }, this.props.children)
                    )
                );
        }
    }

    export var SiteWithSidebar = createClass(SiteWithSidebarSpec);
}
