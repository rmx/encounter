/// <reference path="./Base.ts" />

module rmx.Component {


    export interface InputProps {
        object : any;
        field  : string;
    }

    interface InputState {
        rawValue : string;
    }

    function asString(value) {
        if (value === null || value === undefined) {
            return '';
        } else {
            return value;
        }
    }

    class InputSpec extends ReactComponent<InputProps, InputState> {

        getInitialState() {
            var rawValue = this.props.object[this.props.field];
            return { rawValue : asString(rawValue) };
        }

        render() {
            var valueLink =
                { value: this.state.rawValue
                , requestChange: value => {
                    this.setState({ rawValue: value });
                    this.props.object[this.props.field] = value;
                  }
                };

            return React.DOM.input
                ( { type: 'text', valueLink: valueLink }
                );
        }

        componentWillReceiveProps(nextProps) {
            var rawValue = this.props.object[this.props.field];
            this.setState({ rawValue: asString(rawValue) });
        }
    }

    export var Input = createClass(InputSpec);
}
