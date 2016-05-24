/// <reference path="./Base.ts" />

module rmx.Component {

    export interface CheckboxProps {
        object : any;
        field  : string;
    }

    class CheckboxSpec extends ReactComponent<CheckboxProps, {}> {

        render() {
            var value     = !!this.props.object[this.props.field]
              , iconState = value ? 'checked' : 'empty'
              , className = iconState + ' checkbox icon';

            return React.DOM.div
                ( { className: 'rmx checkbox', onClick: this.toggle }
                , React.DOM.i({ className: className })
                );
        }

        toggle() {
            var value = !!this.props.object[this.props.field];
            this.props.object[this.props.field] = !value;
        }

    }

    export var Checkbox = createClass(CheckboxSpec);
}
