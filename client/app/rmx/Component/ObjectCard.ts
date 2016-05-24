/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Link.ts" />


module rmx.Component {

    export interface ObjectCardProps {
        reference;
        onSelect;
    }

    class ObjectCardSpec extends ReactComponent<ObjectCardProps, {}> {
        render() {
            var ref         = this.props.reference
              , displayName = rmx.data.displayName(ref).get(ref.toString());

            return React.DOM.div
                ( { className: 'rmx object-card' }
                , React.DOM.div
                    ( { className: 'card' }
                    , React.DOM.div
                        ( { className: 'image' }
                        , Link
                            ( { onClick: this.props.onSelect }
                            , React.DOM.img({ src: 'http://placehold.it/160x90' })
                            )
                        , React.DOM.div
                            ( { className: 'rmx link overlay', onClick: this.props.onSelect }
                            , React.DOM.h4({}, ref.objectId)
                            )
                        )
                    )
                , React.DOM.div
                    ( { className: 'display-name' }
                    , displayName
                    )
                );
        }
    }

    export var ObjectCard = createClass(ObjectCardSpec);
}
