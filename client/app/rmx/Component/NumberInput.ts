/// <reference path="./Base.ts" />

declare var classNames;

module rmx.Component {

    export interface NumberInputProps {
        object : any;
        field  : string;
    }

    interface NumberInputState {
        rawValue : string;
    }

    // The value is only valid if it can be fully parsed into a number.
    function isValidNumber(value: string): boolean {
        var num = parseFloat(value);
        return !isNaN(num) && value == '' + num;
    }

    function asString(value) {
        if (value === null || value === undefined) {
            return '';
        } else {
            return value;
        }
    }


    class NumberInputSpec extends ReactComponent<NumberInputProps, NumberInputState> {

        getInitialState() {
            var rawValue = this.props.object[this.props.field];
            return { rawValue : asString(rawValue) };
        }

        render() {
            var valueLink =
                { value: this.state.rawValue
                , requestChange: value => {
                    this.setState({ rawValue: value });
                    if (isValidNumber(value)) {
                        this.props.object[this.props.field] = parseFloat(value);
                    }
                  }
                };

            var className = classNames(
                { invalid : !isValidNumber(this.state.rawValue)
                }
            );

            return React.DOM.input
                ( { type: 'text', className: className, valueLink: valueLink }
                );
        }
    }

    export var NumberInput = createClass(NumberInputSpec);
}
