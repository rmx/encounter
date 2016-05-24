/// <reference path="./NavBar.ts" />
/// <reference path="../data.ts" />``
/// <reference path="../Core/Account.ts" />
/// <reference path="../../entry.ts" />

module rmx.Component {

    export function
    navbarPeers() {
        var peers = [];

        for (var peerId in rmx.app.peerH.peer.connections) {
            var cs = rmx.app.peerH.peer.connections[peerId];
            cs.forEach(mediaConnection => {
                if (mediaConnection.open) {
                    var src = URL.createObjectURL(mediaConnection.remoteStream);
                    if (src !== null) {
                        var avatarUrl = rmx.Core.accountAvatarUrl(mediaConnection.metadata.callerId);

                        function toggleMute() {
                            mediaConnection.remoteStream.getAudioTracks().forEach(track => {
                                track.enabled = !track.enabled;
                            });
                        }

                        peers.push(
                            React.DOM.img
                                ( { src: avatarUrl, className: 'peer', onClick: toggleMute }
                                )
                        );
                    }
                }
            });
        }

        return peers;
    }


    export var navbarSpacer =
        React.DOM.div({ className: 'spacer' });

    export var navbarVerticalSeparator =
        React.DOM.div({ className: 'vertical-separator' });

    var navbarOpenGamesLink =
        React.DOM.a
            ( { className: 'link', href: '/openGames' }
            , 'open games'
            );

    var navbarEncountersLink =
        React.DOM.a
            ( { className: 'link', href: '/' }
            , 'encounters'
            );

    var navbarCall =
        React.DOM.a
            ( { className: 'link', href: '/call' }
            , 'call friends'
            );

    var navbarSettings =
        React.DOM.a
            ( { className: 'link', href: '/settings' }
            , 'settings'
            );


    class AppNavBarSpec extends ReactComponent<{}, {}> {

        render() {
            if (rmx.data.session.accountId) {

                var accountId   = rmx.data.session.accountId
                  , accountName = rmx.Core.accountLogin(accountId);

                return NavBar
                    ( {}
                    , React.DOM.a
                        ( { className: 'link', href: '/settings/profile' }
                        , accountName
                        )
                    , navbarVerticalSeparator
                    , navbarEncountersLink
                    , navbarVerticalSeparator
                    , navbarOpenGamesLink
                    , navbarSpacer
                    , navbarPeers()
                    , navbarCall
                    , navbarSettings
                    , React.DOM.div
                        ( { className: 'inverted button', onClick: this.signout }
                        , 'Sign out'
                        )
                    );

            } else {
                return NavBar
                    ( {}
                    , navbarEncountersLink
                    , navbarVerticalSeparator
                    , navbarOpenGamesLink
                    , navbarSpacer
                    , React.DOM.a
                        ( { className: 'inverted button', href: '/login' }
                        , 'Log in'
                        )
                    , React.DOM.a
                        ( { className: 'button', href: '/signup' }
                        , 'Sign up'
                        )
                    );
            }
        }


        signout() {
            rmx.signout(rmx.data.session);
        }
    }

    export var AppNavBar = createClass(AppNavBarSpec);
}
