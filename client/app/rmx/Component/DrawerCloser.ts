/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    class DrawerCloserSpec extends ReactComponent<{}, {}> {

        render() {
            if (rmx.data.localState.drawer.open) {
                return React.DOM.div(
                    { className : 'rmx drawer-closer'
                    , onClick   : this.onClick
                    }
                );

            } else {
                return null;
            }
        }

        onClick() {
            rmx.data.localState.drawer.open = false;
        }
    }

    export var DrawerCloser = createClass(DrawerCloserSpec);
}
