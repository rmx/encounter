/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {

    export interface FormSectionHeaderProps {
        header : string;
    }

    class FormSectionHeaderSpec extends ReactComponent<FormSectionHeaderProps, {}> {
        render() {
            return React.DOM.div
                ( { className: 'rmx form-section-header' }
                , this.props.header
                , React.DOM.i({ className: 'help icon' })
                );
        }
    }

    export var FormSectionHeader = createClass(FormSectionHeaderSpec);
}
