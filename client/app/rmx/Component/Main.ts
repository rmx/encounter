/// <reference path="./Base.ts" />

module rmx.Component {

    export interface MainProps extends ReactProps {
        className ?: string;
        style?;
    }

    class MainSpec extends ReactComponent<MainProps, {}> {
        render() {
            var className = 'rmx main ' + (this.props.className || '');
            return React.DOM.main
                ( { className: className, style: this.props.style }
                , this.props.children
                );
        }
    }

    export var Main = createClass(MainSpec);
}
