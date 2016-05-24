/// <reference path="./Body.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    class SiteSpec extends ReactComponent<ReactProps, {}> {
        render() {
            var className = 'rmx site';
            if (rmx.data.localState.drawer.open) {
                className += " drawer-open";
            }

            return (
                React.DOM.div
                    ( { className: className }
                    , this.props.children
                    )
            );
        }
    }

    export var Site = createClass(SiteSpec);
}

