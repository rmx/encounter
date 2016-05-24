/// <reference path="./Base.ts" />

module rmx.Component {

    class LoadingScreenSpec extends ReactComponent<ReactProps, {}> {
        render() {
            var content = this.props.children || 'Loading...';

            return (
                React.DOM.div
                    ( { className: 'rmx loading-screen' }
                    , React.DOM.div({ className: 'content' }, content)
                    )
            );
        }
    }

    export var LoadingScreen = createClass(LoadingScreenSpec);
}
