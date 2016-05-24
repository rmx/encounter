/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {


    function statusText(): string {
        if (rmx.data.numSavingEntities > 0) {
            return 'saving...';
        } else if (rmx.data.numUnsavedChanges > 0) {
            var text = '' + rmx.data.numUnsavedChanges + ' unsaved changes';
            if (rmx.data.numEntitiesWithErrors > 0) {
                return text + ' (' + rmx.data.lastErrorMessage + ')';
            } else {
                return text;
            }
        }
    }

    class SavingIndicatorSpec extends ReactComponent<{}, {}> {
        render() {
            // To style the indicator, it's easiest to add `|| 'text'` to the
            // line just below.
            var text = statusText();

            if (text) {
                return (
                    React.DOM.div
                        ( { className: 'rmx saving-indicator' }
                        , text
                        )
                );

            } else {
                return null;
            }
        }
    }

    export var SavingIndicator = createClass(SavingIndicatorSpec);
}


