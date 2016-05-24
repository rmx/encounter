/// <reference path="./Base.ts" />
/// <reference path="../../entry.ts" />

module rmx.Component {

    class BodySpec extends ReactComponent<ReactProps, {}> {
        render() {
            var peerStreams = [];

            // Peer may not be available (client is offline).
            if (rmx.app.peerH.peer) {
                for (var peerId in rmx.app.peerH.peer.connections) {
                    var cs = rmx.app.peerH.peer.connections[peerId];
                    cs.forEach(mediaConnection => {
                        if (mediaConnection.open) {
                            var src = URL.createObjectURL(mediaConnection.remoteStream);
                            if (src !== null) {
                                peerStreams.push(React.DOM.audio
                                    ( { key: mediaConnection.id, src: src, autoPlay: true }
                                    )
                                );
                            }
                        }
                    });
                }
            }

            return React.DOM.div
                ( { className: 'rmx body' }
                , peerStreams
                , this.props.children
                );
        }
    }

    export var Body = createClass(BodySpec);
}
