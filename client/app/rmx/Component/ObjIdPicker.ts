/// <reference path="../../ext/computation.ts" />

/// <reference path="../data.ts" />
/// <reference path="../picker.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Link.ts" />

module rmx.Component {

    export interface ObjIdPickerProps {
        object       : any;
        field        : string;
        searchResult : Computation<rmx.Storage.Reference[]>;
    }

    class ObjIdPickerSpec extends ReactComponent<ObjIdPickerProps, {}> {
        render() {
            var objId       = this.props.object[this.props.field]
              , ref         = rmx.data.objIdReference(objId)
              , displayName = rmx.data.displayName(ref).get('Not selected');

            var className = 'rmx reference-picker';
            if (!objId) {
                className += ' invalid';
            }

            var displayNameElement;
            if (objId) {
                displayNameElement = Link
                    ( { href: '/o/' + objId, className: 'display-name' }
                    , displayName
                    );

            } else {
                displayNameElement = React.DOM.div
                    ( { className: 'display-name' }
                    , displayName
                    );
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
            this.props.object[this.props.field] = ref.objectId;
            rmx.picker.dismiss();
        }

        clear() {
            this.props.object[this.props.field] = null;
        }
    }

    export var ObjIdPicker = createClass(ObjIdPickerSpec);
}
