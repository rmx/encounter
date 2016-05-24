/// <reference path="./Base.ts" />

declare var classNames;

module rmx.Component {

    export interface VectorInputProps {
        object : any;
        field  : string;
    }

    interface VectorInputState {
        rawValue : string;
    }


    function tryParse(rawValue) {
        if (rawValue.trim() == "") {
            return [];
        }

        var elements = rawValue.split(',')
          , value    = elements.map(parseFloat);

        var isValid = value.every(function(x, index) {
            return !isNaN(x) && elements[index] == x;
        });

        if (isValid) {
            return value;
        }
    }


    class VectorInputSpec extends ReactComponent<VectorInputProps, VectorInputState> {

        getInitialState() {
            var rawValue = this.props.object[this.props.field];
            return { rawValue : rawValue.join(',') };
        }

        render() {
            var valueLink =
                { value: this.state.rawValue
                , requestChange: value => {
                    this.setState({ rawValue: value });
                    var newValue = tryParse(value);
                    if (newValue !== undefined) {
                        this.props.object[this.props.field] = newValue;
                    }
                  }
                };

            var isValid = undefined !== tryParse(this.state.rawValue);
            var className = classNames(
                { invalid : !isValid
                }
            );

            return React.DOM.textarea
                ( { className: className, valueLink: valueLink }
                );
        }
    }

    export var VectorInput = createClass(VectorInputSpec);
}
