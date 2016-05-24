/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />
/// <reference path="../../entry.ts" />

module rmx.Component {

    export interface LinkProps {
        href      ?: string;
        onClick   ?: Function;
        children  ?: any;
        className ?: string;
    }

    class LinkSpec extends ReactComponent<LinkProps, {}> {
        render() {
            var className = 'rmx link';
            if (window.location.pathname === this.props.href) {
                className += ' active';
            }

            if (this.props.className) {
                className += ' ' + this.props.className;
            }

            return React.DOM.div
                ( { className: className, onClick: this.onClick }
                , this.props.children
                );
        }

        onClick(e) {
            rmx.app.navigateToFn(this.props.href)(e);
            if (this.props.onClick) {
                this.props.onClick(e);
            }
        }
    }

    export var Link = createClass(LinkSpec);
}
