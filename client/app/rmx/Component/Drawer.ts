/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    class DrawerSpec extends ReactComponent<{ children?: any }, {}> {

        render() {
            return React.DOM.div
                ( { className: 'rmx drawer' }
                , this.props.children
                );
        }
    }

    export var Drawer = createClass(DrawerSpec);
}
