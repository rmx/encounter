/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {

    export interface CollectionItemHeaderProps {
        id        : string;
        text      : string;
        onRemove ?: any;
        onSelect ?: Function;
    }

    class CollectionItemHeaderSpec extends ReactComponent<CollectionItemHeaderProps, {}> {
        render() {

            var className = 'rmx collection-item-header';
            if (this.props.onSelect) {
                className += ' selectable';
            }

            return React.DOM.div
                ( { className: className }
                , React.DOM.div({ className: 'id', onClick: this.onClick }, this.props.id)
                , this.props.text
                , React.DOM.i({ className: 'remove icon link', onClick: this.props.onRemove })
                );
        }

        onClick(ev) {
            if (this.props.onSelect) {
                this.props.onSelect();
            }
        }
    }

    export var CollectionItemHeader = createClass(CollectionItemHeaderSpec);
}
