/// <reference path="./Body.ts" />
/// <reference path="./Site.ts" />
/// <reference path="./SavingIndicator.ts" />
/// <reference path="./MainNav.ts" />
/// <reference path="./DrawerCloser.ts" />

module rmx.Component {

    class PageSpec extends ReactComponent<ReactProps, {}> {
        render() {
            return Body
                ( {}
                , Site
                    ( {}
                    , SavingIndicator()
                    , MainNav()
                    , DrawerCloser()
                    , this.props.children
                    )
                );
        }
    }

    export var Page = createClass(PageSpec);
}
