/// <reference path="./Base.ts" />
/// <reference path="./TooltipMixin.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    export interface IconImageProps extends ReactProps {
        iconId : string;
        onClick?;
    }

    class IconImageSpec extends ReactComponent<IconImageProps, {}> {

        render() {
            var url = rmx.data.iconUrl(this.props.iconId);

            return React.DOM.div
                ( { className: 'rmx icon-image', onClick: this.props.onClick }
                , React.DOM.img({ src: url })
                );
        }

        tooltipContent() {
            if (this.props.children) {
                return React.DOM.div({}, this.props.children);
            }
        }
    }

    IconImageSpec.prototype.mixins =
        [ TooltipMixin
        ];


    export var IconImage = createClass(IconImageSpec);
}
