/// <reference path="../data.ts" />
/// <reference path="../Editor/ModelRenderer.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {

    export interface ModelCanvasState {
        renderer: rmx.Editor.ModelRenderer;
    }

    export interface ModelCanvasProps {
        model         : rmx.Storage.Model;
        animationName : string;
        className    ?: string;
    }

    class ModelCanvasSpec extends ReactComponent<ModelCanvasProps, ModelCanvasState> {

        getInitialState() {
            return { renderer: new rmx.Editor.ModelRenderer };
        }

        render() {
            var mr           = this.state.renderer;
            mr.model         = this.props.model;
            mr.animationName = this.props.animationName;

            var className = 'rmx model-canvas ' + (this.props.className || '');

            return React.DOM.div({ className: className, ref: 'canvas' });
        }

        componentDidMount() {
            var canvas   = <HTMLElement> this.refs['canvas'].getDOMNode()
              , renderer = this.state.renderer;

            renderer.scene.appendTo(canvas);
            renderer.inputSource.bindEvents();

            var self = this;
            function render(now) {
                if (self.isMounted()) {
                    requestAnimationFrame(render);
                    renderer.update(now);
                }
            }

            requestAnimationFrame(render);
        }

        componentWillUnmount() {
            var canvas   = <HTMLElement> this.refs['canvas'].getDOMNode()
              , renderer = this.state.renderer;

            renderer.inputSource.unbindEvents();
            renderer.scene.removeFrom(canvas);
        }
    }

    export var ModelCanvas = createClass(ModelCanvasSpec);
}
