/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    export interface ImageProps {
        blobId       : string;
        placeholder ?: string;
    }

    class ImageSpec extends ReactComponent<ImageProps, {}> {
        render() {
            if (this.props.blobId) {
                return React.DOM.img({ src: rmx.blobUrl(this.props.blobId) });

            } else if (this.props.placeholder) {
                return React.DOM.img({ src: this.props.placeholder });

            } else {
                return React.DOM.div();
            }
        }
    }

    export var Image = createClass(ImageSpec);
}
