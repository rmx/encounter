/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    export interface NavBarProps {
        children ?: any;
    }

    class NavBarSpec extends ReactComponent<NavBarProps, {}> {

        render() {
            var logo = React.DOM.a
                ( { href: '/', className: 'logo' }
                , React.DOM.img({ src: rmx.assets.logo })
                );

            var homeIcon = React.DOM.a
                ( { className: 'nav-bar-icon', onClick: this.toggleMenu }
                , React.DOM.i({ className: 'home icon' })
                );

            return React.DOM.nav
                ( { className: 'rmx nav-bar' }
                , logo
                , homeIcon
                , this.props.children
                );
        }

        toggleMenu(): void {
            rmx.data.localState.drawer.open = !rmx.data.localState.drawer.open;
        }
    }

    export var NavBar = createClass(NavBarSpec);
}
