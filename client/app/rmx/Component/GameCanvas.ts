/// <reference path="./Base.ts" />
/// <reference path="../Game.ts" />

module rmx.Component {

    export interface GameCanvasProps {
        client : rmx.Game.Client;
    }

    class GameCanvasSpec extends ReactComponent<GameCanvasProps, {}> {
        render() {
            return React.DOM.div
                ( { className: 'canvas', contentEditable: true, ref: 'canvas' }
                );
        }

        componentDidMount() {
            var canvas = <HTMLElement> this.refs['canvas'].getDOMNode();
            this.props.client.renderer.appendTo(canvas);
        }

        componentWillUnmount() {
            var canvas = <HTMLElement> this.refs['canvas'].getDOMNode();
            this.props.client.renderer.removeFrom(canvas);
        }
    }

    export var GameCanvas = createClass(GameCanvasSpec);
}
