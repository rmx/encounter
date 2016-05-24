/// <reference path="../../data.ts" />
/// <reference path="../../Game.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../Link.ts" />

module rmx.Component.View {

    function deviceLabel(platform: string): string {
        switch (platform) {
        case 'MacIntel': return 'Mac';
        default:         return platform;
        }
    }

    function device(accountId, pce: rmx.Core.Peer.PeerCacheEntry) {
        var label = React.DOM.div({ className: 'label' }, deviceLabel(pce.device.platform));
        var seenAt = rmx.Component.Time({ datetime: pce.updatedAt });

        var connections = rmx.Core.Peer.lookupPeerConnections(rmx.app.peerH, pce.id)
          , c           = <any> connections[0];

        function call() {
            rmx.app.userMediaStreamPromise().then(stream => {
                rmx.Core.Peer.openPeerConnection
                    ( rmx.app.peerH
                    , stream
                    , accountId
                    , pce.id
                    );
            });
        }

        if (c) {
            if (c.remoteStream && c.remoteStream.open) {
                return React.DOM.div
                    ( { className: 'link rmx peer-device calling', key: pce.id }
                    , label, seenAt
                    );

            } else {
                return React.DOM.div
                    ( { className: 'link rmx peer-device open', key: pce.id }
                    , label, seenAt
                    );
            }

        } else {
            return React.DOM.div
                ( { className: 'link rmx peer-device', onClick: call, key: pce.id }
                , label, seenAt
                );
        }
    }

    class CallSpec extends ReactComponent<{}, {}> {
        render() {
            var friends = rmx.data.objectContent<rmx.Storage.Account>(<string>rmx.data.session.accountId)
            .fmap(account => {
                return account.friends.map(id => {
                    return rmx.data.objectContent<rmx.Storage.Account>(id).fmap(friend => {
                        var peers = rmx.Core.Peer.peersOf(rmx.app.peerH, id).fmap(pces => {
                            return pces.map(pce => {
                                return device(id, pce);
                            });
                        }).get([]);

                        if (peers.length > 0) {
                            return React.DOM.div
                                ( { key: id }
                                , React.DOM.h2({}, friend.login)
                                , React.DOM.div
                                    ( { className: 'rmx peer-devices' }
                                    , peers
                                    )
                                );
                        }
                    }).get(undefined);
                });
            }).get([]);

            return Page
                ( {}
                , AppNavBar()
                , Main
                    ( { className: 'vertical' }
                    , React.DOM.div
                        ( {}
                        , friends
                        )
                    )
                );
        }
    }

    export var Call = createClass(CallSpec);
}
