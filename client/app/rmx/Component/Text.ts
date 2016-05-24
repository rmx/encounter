/// <reference path="./Base.ts" />

module rmx.Component {


    export interface TextProps {
        object : any;
        field  : string;
    }

    interface TextState {
        rawValue : string;
    }

    function asString(value) {
        if (value === null || value === undefined) {
            return '';
        } else {
            return value;
        }
    }

    class TextSpec extends ReactComponent<TextProps, TextState> {

        getInitialState() {
            var rawValue = this.props.object[this.props.field];
            return { rawValue : asString(rawValue) };
        }

        render() {
            return React.DOM.textarea
                ( { value: this.state.rawValue, onChange: this.onChange }
                );
        }

        onChange(e) {
            var value = e.target.value;

            this.setState({ rawValue: value });
            this.props.object[this.props.field] = value;
        }

        componentWillReceiveProps(nextProps) {
            var rawValue = this.props.object[this.props.field];
            this.setState({ rawValue : asString(rawValue) });
        }
    }

    export var Text = createClass(TextSpec);
}
