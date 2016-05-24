/// <reference path="./ext/avers.ts" />
/// <reference path="./ext/computation.ts" />
/// <reference path="./ext/three.d.ts" />
/// <reference path="./ext/underscore.ts" />

/// <reference path="./rmx/Pure/Protocol.ts" />
/// <reference path="./rmx/Pure/Message.ts" />
/// <reference path="./rmx/Pure/Game.ts" />
/// <reference path="./rmx/Pure/String.ts" />

/// <reference path="./rmx/data.ts" />
/// <reference path="./routes.ts" />
/// <reference path="./rmx/paths.ts" />

/// <reference path="./rmx/ParticleEffectRenderer.ts" />
/// <reference path="./rmx/Editor/ModelRenderer.ts" />

/// <reference path="./rmx/Components.ts" />
/// <reference path="./rmx/Component.ts" />

/// <reference path="./rmx/Core/Audio.ts" />
/// <reference path="./rmx/Core/Peer.ts" />



interface ReactComponent {}

// Need to set the paths to the Ace coffee worker because it is being loaded
// at runtime into a worker.

declare var ace: any;
var aceConfig = ace.require('ace/config');
aceConfig.setModuleUrl('ace/mode/coffee_worker', rmx.assets.ace.coffeeWorkerUrl);



module rmx {

    // A View describes a particular view on the rmx state. Currently it only
    // provides means to create a React component which describes the main
    // UI body.

    export class View {

        createBodyComponent : () => ReactComponent;

        constructor(createBodyComponent: () => ReactComponent) {
            this.createBodyComponent = createBodyComponent;
        }
    }
}


interface IHeaderTitle {
    mainTitle: string;
    contextHref: string;
    contextLabel: string;
}

module rmx {

    // Tooltip
    // -----------------------------------------------------------------------

    export class Tooltip {

        impl : any = new (<any>window).Tooltip('', { auto: 1 });
        // ^ The tooltip implementation.

        owner : any = null;
        // ^ React component which currently owns the tooltip.
    }



    // app
    // -----------------------------------------------------------------------
    //
    // This module contains the essential state of the app. Well, everything
    // that is not managed by rmx.data.

    export module app {

        export var view: rmx.View = null;
        // ^ The view which is currently active and is used to render the UI.
        // The view is updated using the 'loadView' function. The view is
        // calculated from the URL. The mapping from URL to view is defined
        // in the routes.

        export var viewCounter: number = 0;
        // ^ A counter which is incremented whenever the UI transitions from
        // one view to another. This is required so that overlapping
        // transitions work as expected.

        export var audio: rmx.Core.Audio.System = new rmx.Core.Audio.System();
        // ^ The global audio system.

        export var tooltip : rmx.Tooltip = new rmx.Tooltip;
        // ^ Tooltip state.

        export var peerH = new rmx.Core.Peer.Handle(rmx.data.session);
        // ^ The Peer Handle manages P2P connections (MediaStream / Data)
        // between clients.


        var _userMediaStream = undefined;
        // ^ The global user media stream which is used to call / answer peers.

        export function userMediaStreamPromise(): Promise<any> {
            if (_userMediaStream === undefined) {
               _userMediaStream = new Promise((resolve, reject) => {
                    navigator.webkitGetUserMedia({ audio: true, video: false }, stream => {
                        _userMediaStream = stream;
                        resolve(stream);
                    }, reject);
               });
            }

            if (_userMediaStream instanceof Promise) {
                return _userMediaStream;
            } else {
                return Promise.resolve(_userMediaStream);
            }
        }

        export function userMediaStream(): Computation<any> {
            return new Computation(() => {
                userMediaStreamPromise();

                if (_userMediaStream === undefined) {
                    return Computation.Pending;
                } else if (_userMediaStream instanceof Promise) {
                    return Computation.Pending;
                } else {
                    return _userMediaStream;
                }
            });
        }


        // loadView
        // -------------------------------------------------------------------
        //
        // Transition to a new view. This is called from the routing code after
        // the location (path) changes.

        export interface MkViewCallback {
            (nextView: rmx.View, audioHandle: rmx.Core.Audio.Handle): void;
        }

        export function
        loadView(mkView: (cb: MkViewCallback) => void): void {
            viewCounter += 1;
            var counter = viewCounter;

            // Mark the beginning of transition to a new view. We add a class to the
            // body so that the existing view can react to the upcoming change.
            document.body.classList.add('loading');

            // The function asynchronously creates the view and invokes the
            // callback with it.
            mkView((nextView, audioHandle) => {
                if (counter === viewCounter) {
                    view = nextView;
                    rmx.Core.Audio.allowHandle(audio, audioHandle);

                    React.render(createBodyComponent(), document.body, () => {
                        if (counter === viewCounter) {
                            // Mark the end of the transition.
                            document.body.classList.remove('loading');
                        }
                    });
                }
            });
        }


        // refresh
        // -------------------------------------------------------------------
        //
        // Refresh the UI by rendering the view again.

        var refreshRequestId = undefined;
        export function
        refresh(): void {
            if (refreshRequestId === undefined) {
                refreshRequestId = requestAnimationFrame(refreshCallback);
            }
        }

        function refreshCallback() {
            refreshRequestId = undefined;
            if (view) {
                React.render(createBodyComponent(), document.body);
            }
        }

        function
        createBodyComponent(): any {
            if (!Object.observe) {
                return rmx.Components.BrowserWarning();
            } else {
                return view.createBodyComponent();
            }
        }




        // navigateTo
        // -------------------------------------------------------------------
        //
        // Change the active path. Delegates to the underlying library which
        // we use (currently page.js).

        export function
        navigateTo(path: string): void {

            // FIXME: This dependency here smells. The router should clear
            // the tooltip before changing the view.
            //
            // Actually, the react component should dismiss the tooltip
            // on unmount.

            rmx.app.tooltip.impl.detach().hide();
            rmx.app.tooltip.owner = null;


            requestAnimationFrame(function() {
                page(path);
                rmx.data.startNextGeneration();
            });
        }



        // navigateToFn
        // -------------------------------------------------------------------
        //
        // Returns a function which when called will navigate to the given
        // path. This is expected to be passed to react event handlers like
        // 'onClick'.

        export function
        navigateToFn(path: string): (e: any) => void {
            return function(e) {
                e.preventDefault();
                navigateTo(path);
            };
        }
    }


    export function bootstrap() {
        Routes.setupRoutes();
        rmx.restoreSession(rmx.data.session);

        if (Object.observe) {
            Object.observe(rmx.data, function dataGenerationChange(records) {
                var changedGeneration = records.some(rec => {
                    return rec.name == 'generationNumber';
                });

                if (changedGeneration) {
                    rmx.app.refresh();
                }
            });
        }

        parallel([domContentLoaded, sessionStatusDetermined], function() {
            page();
        });
    }
}


function parallel(callbacks: Function[], then: Function): void {
    var i = callbacks.length;
    function done() {
        if (--i === 0) {
            then();
        }
    }

    callbacks.forEach(function(callback) {
        callback(done);
    });
}

function domContentLoaded(callback: any) {
    document.addEventListener('DOMContentLoaded', callback);
}

interface Object {
    observe(object: any, fn: Function): void;
    unobserve(object: any, fn: Function): void;
}

// Can't remove this just yet. The routing code is not re-run when the data
// generation changes, but it would need to be because it returns a different
// view depending on the session status.
function sessionStatusDetermined(callback: Function) {
    function onChange(records) {
        records.forEach(function(rec) {
            if (rec.name == 'status') {
                var newValue = rmx.data.session.status;

                switch (newValue) {
                case rmx.SessionStatus.Anonymous:
                case rmx.SessionStatus.Authenticated:
                case rmx.SessionStatus.Error:
                    Object.unobserve(rmx.data.session, onChange);
                    callback();
                }
            }
        });
    }

    Object.observe(rmx.data.session, onChange);
}
