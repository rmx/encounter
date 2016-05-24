/// <reference path="./Base.ts" />
/// <reference path="../../entry.ts" />

module rmx.Component {

    function tooltipContent(x) {
        return x.tooltipContent();
    }

    class TooltipMixinSpec extends ReactComponent<{}, {}> {

        // Implemented by the component which uses this mixin.
        tooltipContent;

        componentDidMount() {
            var el = this.getDOMNode();

            el.addEventListener('mouseenter', this.mouseenter, false);
            el.addEventListener('mouseleave', this.mouseleave, false);
        }

        componentDidUpdate() {
            if (!rmx.app.tooltip.impl.hidden && rmx.app.tooltip.owner === this && this.tooltipContent) {
                this.update(tooltipContent(this));
            }
        }

        componentWillUnmount() {
            var el = this.getDOMNode();

            el.removeEventListener('mouseenter', this.mouseenter);
            el.removeEventListener('mouseleave', this.mouseleave);

            // As a saftery precaution, destroy the tooltip if the owner
            // is unmounted. I suspect the browser won't emit a 'mouseleave'
            // event which could leave the tooltip dangling.
            if (rmx.app.tooltip.owner === this) {
                rmx.app.tooltip.impl.detach().hide();
                rmx.app.tooltip.owner = null;
            }
        }

        mouseenter() {
            rmx.app.tooltip.owner = this;
            if (this.tooltipContent) {
                this.update(tooltipContent(this));
            } else {
                console.warn('Component has TooltipMixin but does not provide tooltipContent()', this);
            }
        }

        mouseleave() {
            if (rmx.app.tooltip.owner === this) {
                rmx.app.tooltip.impl.detach().hide();
                rmx.app.tooltip.owner = null;
            }
        }

        update(content) {
            var el = this.getDOMNode();
            React.render(content, rmx.app.tooltip.impl.element, function() {
                rmx.app.tooltip.impl.attach(el).show().updateSize().place('top');
            });
        }
    }

    export var TooltipMixin = createMixin(TooltipMixinSpec);
}
