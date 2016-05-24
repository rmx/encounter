/// <reference path="../data.ts" />
/// <reference path="../picker.ts" />
/// <reference path="../../entry.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Link.ts" />

module rmx.Component {

    export interface ReferencePickerProps {
        reference    : rmx.Storage.Reference;
        searchResult : Computation<rmx.Storage.Reference[]>;

        toPath      ?: (ref: rmx.Storage.Reference) => string;
    }

    class ReferencePickerSpec extends ReactComponent<ReferencePickerProps, {}> {
        render() {
            var ref         = this.props.reference
              , displayName = rmx.data.displayName(ref).get('Not selected');

            var className = 'rmx reference-picker';
            if (ref.toString() === undefined) {
                className += ' invalid';
            }

            var displayNameElement;
            if (this.props.toPath && this.props.toPath(ref)) {
                displayNameElement = Link
                    ( { href: this.props.toPath(ref), className: 'display-name' }
                    , displayName
                    );

            } else {
                displayNameElement = React.DOM.div({ className: 'display-name' }, displayName);
            }

            return React.DOM.div
                ( { className: className }
                , displayNameElement
                , React.DOM.div({ className: 'small danger button', onClick: this.clear }, 'Clear')
                , React.DOM.div({ className: 'small button', onClick: this.showPicker }, 'Change')
                );
        }

        showPicker() {
            rmx.picker.initialize
              ( window.location.pathname
              , 'Back to ???'
              , this.props.searchResult
              , this.set
              );

            rmx.app.navigateTo('/picker');
        }

        set(ref) {
            this.props.reference.objectId   = ref.objectId;
            this.props.reference.revisionId = ref.revisionId;
            this.props.reference.path       = ref.path;

            rmx.picker.dismiss();
        }

        clear() {
            this.props.reference.objectId   = null;
            this.props.reference.revisionId = null;
            this.props.reference.path       = null;
        }
    }

    export var ReferencePicker = createClass(ReferencePickerSpec);
}
