/// <reference path="./Base.ts" />
/// <reference path="../Storage/Types.ts" />

module rmx.Component {

    export interface ReferenceProps {
        reference : rmx.Storage.Reference;
    }

    class ReferenceSpec extends ReactComponent<ReferenceProps, {}> {

        render() {
            var ref = this.props.reference;

            var objectIdLink =
                { value: ref.objectId
                , requestChange: function(value) {
                    ref.objectId = value;
                  }
                };

            var pathLink =
                { value: ref.path
                , requestChange: function(value) {
                    ref.path = value;
                  }
                };

            return React.DOM.div
                ( { className: 'form-row' }

                , React.DOM.div
                    ( { className: 'form-row-element' }
                    , React.DOM.label({}, 'object id')
                    , React.DOM.input({ valueLink: objectIdLink })
                    )

                , React.DOM.div
                    ( { className: 'form-row-element' }
                    , React.DOM.label({}, 'path')
                    , React.DOM.input({ valueLink: pathLink })
                    )

                );
        }
    }

    export var Reference = createClass(ReferenceSpec);
}
