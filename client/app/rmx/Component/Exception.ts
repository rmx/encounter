/// <reference path="./Base.ts" />

module rmx.Component {

    export interface ExceptionProps {
        error : Error;
    }

    class ExceptionSpec extends ReactComponent<ExceptionProps, {}> {
        render() {
            return React.DOM.div
                ( { className: 'rmx exception' }
                , React.DOM.div({ className: 'header' }, 'Oops, the client crashed')
                , React.DOM.p({ className: 'description' }, this.props.error.message)
                );
        }
    }

    export var Exception = createClass(ExceptionSpec);
}
