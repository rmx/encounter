/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {

    class ObjectCardCollectionSpec extends ReactComponent<ReactProps, {}> {
        render() {
            return React.DOM.div
                ( { className: 'rmx object-card-collection' }
                , this.props.children
                );
        }
    }

    export var ObjectCardCollection = createClass(ObjectCardCollectionSpec);
}
