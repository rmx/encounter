/// <reference path="../../ext/peerjs.d.ts" />
/// <reference path="../data.ts" />
/// <reference path="../../entry.ts" />

declare var fetch;

module rmx.Core.Peer {

    export class Handle {

        peer        : PeerJs.Peer = undefined;
        syncTimeout : number      = undefined;

        peerRegistryCache = new Map<string, rmx.data.Volatile<PeerCacheEntry[]>>();
        // ^ Cache for map from ObjId to Peer(s).

        constructor(public session: rmx.Session) {
            reconnectPeer(this);
            registerPeer(this);

            Avers.attachChangeListener(session, () => {
                registerPeer(this);
            });
        }
    }

    export interface PeerCacheEntry {
        id        : string;
        device    : PeerDevice;
        updatedAt : string; // ISO8601
    }

    export interface PeerDevice {
        userAgent : string;
        platform  : string;
    }


    // registerPeer
    // -------------------------------------------------------------------------
    //
    // This sends the mapping between the session (accountId) and the PeerJS
    // peerId to the server. Other clients can retrieve this mapping and connect
    // to this client.
    //
    // This process is run continuously in the background, because the server
    // expires old peer mappins after a certain timeout. This is needed because
    // we can't expect clients to deregister when they are closed.

    function registerPeer(h: Handle): void {
        clearTimeout(h.syncTimeout);
        h.syncTimeout = undefined;

        if (h.session.accountId && h.peer.id) {
            // We send along information about the current device. In case the
            // user has multiple clients open, others can select to which to
            // connect.
            var device =
                { userAgent : window.navigator.userAgent
                , platform  : window.navigator.platform
                };

            var data =
                { objId  : h.session.accountId
                , peerId : h.peer.id
                , device : device
                };

            var options =
                { method      : 'POST'
                , credentials : 'include'
                , body        : JSON.stringify(data)
                };

            // TODO: Use Avers.Handle.fetch when we switch to avers.handle.ts.
            fetch(rmx.apiUrl('/peers'), options).then(res => {
                // Peer registration updated (successfully?).
                // console.log('registerPeer', res.status);

            }).catch(err => {
                // In case of an error there is not much we can do, other than
                // retry again.
                console.error('registerPeer', err);

            }).then(() => {
                h.syncTimeout = setTimeout(() => {
                    registerPeer(h);
                }, 5000);
            });

        } else {
            h.syncTimeout = setTimeout(() => {
                registerPeer(h);
            }, 5000);
        }
    }


    // reconnectPeer
    // -------------------------------------------------------------------------
    //
    // We try to keep the peer connected at all times. This means we
    // automatically reconnect when the connection closes.

    function
    reconnectPeer(h: Handle): void {
        var options =
            { key    : 'lfjbgguv72o11yvi'
            , host   : 'api.rmx.im'
            , port   : '443'
            , path   : '/peerjs/'
            , secure : true
            };

        h.peer = new (<any>window).Peer(options);
        rmx.data.startNextGeneration();

        h.peer.on('open', id => {
            console.log('reconnectPeer: id', id);

            // Now that we have the peer ID, try to immediately register it.
            registerPeer(h);

            rmx.data.startNextGeneration();
        });

        h.peer.on('call', mediaConnection => {
            rmx.app.userMediaStreamPromise().then(stream => {
                console.log('reconnectPeer: answering call');
                mediaConnection.answer(stream);
                rmx.data.startNextGeneration();
            });

            registerMediaConnection(h, mediaConnection);
        });

        h.peer.on('close', () => {
            console.log('reconnectPeer: closed connection');

            h.peer = undefined;
            rmx.data.startNextGeneration();

            // TODO: Ratelimit reconnection attempts.
            setTimeout(() => {
                reconnectPeer(h);
            }, 500);
        });
    }

    function registerMediaConnection(h: Handle, mediaConnection): void {
        mediaConnection.on('stream', () => {
            rmx.data.startNextGeneration();
        });

        mediaConnection.on('close', () => {
            console.log('openConnection: media connection closed');
            rmx.data.startNextGeneration();
        });

        mediaConnection.on('error', err => {
            console.error('openConnection:', err);
            rmx.data.startNextGeneration();
        });
    }


    export function
    peersOf(h: Handle, objId: string): Computation<PeerCacheEntry[]> {
        return new Computation<rmx.data.Volatile<PeerCacheEntry[]>>(() => {
            var p = h.peerRegistryCache.get(objId);

            if (!p) {
                p = new rmx.data.Volatile(() => {
                    return fetchPeersOf(objId);
                });
                h.peerRegistryCache.set(objId, p);
            }

            return p;
        }).bind(rmx.data.volatileValue);
    }



    export function
    fetchPeersOf(objId: string): Promise<PeerCacheEntry[]> {
        var url = rmx.apiUrl('/peers/' + objId);
        return fetch(url).then(res => {
            return res.json();
        });
    }


    export function
    lookupConnections(h: Handle, objId): any[] {
        return [];
    }


    export function
    lookupPeerConnections(h: Handle, peerId: string): PeerJs.MediaConnection[] {
        return h.peer.connections[peerId] || [];
    }

    export function
    hasConnection(h: Handle, objId: rmx.Pure.AccountId): boolean {
        for (var peerId in h.peer.connections) {
            var cs = h.peer.connections[peerId];
            for (var i in cs) {
                var c  = cs[i]
                  , md = c.metadata || {};

                if (c.type === 'media' && (md.callerId === objId || md.calleeId === objId)) {
                    return true;
                }
            }
        }

        return false;
    }


    export function
    openConnection
    ( h      : Handle
    , stream : any
    , objId  : rmx.Pure.AccountId
    ): void {
        var peers = peersOf(h, <string>objId).get([]);
        if (peers.length > 0) {
            var peerId = peers[0].id;
            openPeerConnection(h, stream, objId, peerId);
        }
    }

    export function
    openPeerConnection
    ( h      : Handle
    , stream : any
    , objId  : rmx.Pure.AccountId
    , peerId : string
    ): void {
        var metadata =
            { callerId : rmx.data.session.accountId
            , calleeId : objId
            };

        console.log('openPeerConnection: calling', objId, peerId);

        var mediaConnection = h.peer.call(peerId, stream, { metadata: metadata });
        if (mediaConnection) {
            registerMediaConnection(h, mediaConnection);
        } else {
            // console.error('openPeerConnection: peer.call failed');
        }

        rmx.data.startNextGeneration();
    }
}
