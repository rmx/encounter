/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />

/// <reference path="./Script.ts" />


module rmx.Component.Editor {

    export interface ExpressionState {
        showHelp: boolean;
    }

    export interface ExpressionProps {
        expr : rmx.Storage.Expression;
        env  : string;
    }

    class ExpressionSpec extends ReactComponent<ExpressionProps, ExpressionState> {
        getInitialState() {
            return { showHelp: false };
        }

        render() {
            var expr = this.props.expr
              , env  = this.props.env || 'Global';

            return React.DOM.div
                ( { className: 'rmx expression' }
                , React.DOM.div
                    ( { className: 'env' }
                    , env
                    , React.DOM.div({ className: 'help-toggle', onClick: this.toggleHelp }, 'Help')
                    , this.envDocumentation()
                    )
                , React.DOM.div
                    ( { className: 'source' }
                    , Script({ object: expr, field: 'source' })
                    )
                );
        }

        toggleHelp() {
            this.setState({ showHelp: !this.state.showHelp });
        }

        envDocumentation() {
            if (this.state.showHelp) {
                return React.DOM.div
                    ( { className: 'help' }
                    , 'TODO: documentation for the environment.'
                    );

            } else {
                return React.DOM.div();
            }
        }
    }

    export var Expression = createClass(ExpressionSpec);
}
