/// <reference path="../../ext/waa.d.ts" />

/// <reference path="../Pure/Terrain.ts" />
/// <reference path="../data.ts" />


declare var AudioBuffer;


module rmx.Core.Audio {

    // Cache for the audio buffers, indexed by the blobId. These are used to
    // initialize the 'source' property of 'AudioBufferSourceNode' nodes.
    //
    // The value is a Promise<ArrayBuffer> while the buffer is being loaded,
    // and once it is ready the value becomes the buffer itself. This is so
    // that we can access the buffer synchronously (Promise#then is async!).

    var bufferCache : Map<string, any> = new Map<string, any>();



    export class System {

        context : AudioContext;
        // ^ The 'Web Audio' context.

        mainVolume : GainNode;
        // ^ The main volume. There usually is only one 'AudioH' for the whole
        // app. In our case it's part of 'rmx.data'.

        handles : WeakSet<Handle>;
        // ^ All handles which are allowed to play sounds.

        soundNodes : Map<string, Node>;
        // ^ Sound nodes which are currently playing.
        // TODO: Make it per-handle.


        constructor() {
            this.context    = new AudioContext;

            this.mainVolume = this.context.createGain();
            this.mainVolume.connect(this.context.destination);

            this.handles    = new WeakSet<Handle>();
            this.soundNodes = new Map<string, Node>();
        }
    }

    export function
    allowHandle(system: System, handle: Handle): void {
        system.handles = new WeakSet<Handle>();

        if (handle) {
            system.handles.add(handle);
        }

        system.soundNodes.forEach(node => {
            if (!system.handles.has(node.handle)) {
                disposeNode(node);
            }
        });
    }



    export class Handle {

        generationNumber : number;

        constructor(public system: System) {
            this.generationNumber = 1;
        }
    }



    // allocateHandle
    // -----------------------------------------------------------------------
    //
    // Create a new handle which can be used to play sounds. Don't forget to
    // enable the handle in the system, otherwise the sounds won't be audible!

    export function
    allocateHandle(system: System): Handle {
        return new Handle(system);
    }


    export function
    startNextGeneration(handle: Handle): void {
        handle.generationNumber += 1;
    }


    export function
    cleanupHandle(handle: Handle): void {
        var gen = handle.generationNumber;

        handle.system.soundNodes.forEach(node => {
            if (node.handle === handle && node.generationNumber !== gen) {
                disposeNode(node);
            }
        });
    }


    // playSound
    // -----------------------------------------------------------------------
    //
    // Play a sound from the given url.
    //
    // The audio buffer will be loaded if not already available in the local
    // cache. The sound may therefore be played back after a short delay. This
    // shouldn't cause any problems for UI sounds for which it is not
    // necessary that they be played at an exact time.

    export function
    playSound
    ( handle : Handle
    , id     : string
    , url    : string
    ): void {
        loadBuffer(handle.system.context, url).then(buffer => {
            soundNode
                ( handle
                , id
                , url
                , false // oneshot
                , null  // position
                );
        });
    }


    export function
    placeSound
    ( handle   : Handle
    , id       : string
    , soundId  : string
    , oneshot  : boolean
    , position : Vec3
    ): Node {
        var sound = rmx.data.findById<rmx.Storage.Sound>(soundId).get(null);

        if (sound) {
            return soundNode
                ( handle
                , id
                , rmx.blobUrl(sound.content.blobId)
                , oneshot
                , position
                );
        }
    }




    // Node
    // -----------------------------------------------------------------------

    export enum State { Playing, Stopping };

    export class Node {
        constructor
          ( public handle           : Handle
            // ^ The handle for which this node was created.

          , public id               : string
            // ^ An identifier for the node, unique within the handle.

          , public state            : State
            // ^ Used to track whether the sound is playing or in the process
            // of being stopped (which can last a few milliseconds during
            // which the sound is slowly faded out).

          , public oneshot          : boolean
            // ^ True if the sound should play to the end and then be
            // automatically disposed. If this is false, then the node is
            // disposed based on the generation number.

          , public generationNumber : number
            // ^ The generation when this node was last updated at. This is
            // used to retire old sound nodes when they are no longer needed.

          , public bufferSource     : AudioBufferSourceNode
            // ^ The node which represents the source.

          , public volume           : GainNode
            // ^ Currently unused. But could be used to adjust the volume
            // of this sound node.

          , public position         : Vec3
            // ^ Optional position. If set then the sound is routed through
            // a panner node.

          , public panner           : PannerNode
            // ^ If position is set, this is the panner node through which the
            // sound is routed.
          ) {}
    }



    function
    disposeNode(node: Node): void {
        var currentTime = node.handle.system.context.currentTime;

        var offset = 0;
        if (node.oneshot) {
            offset = 2;
        }

        node.volume.gain.linearRampToValueAtTime(1, currentTime + offset);
        node.volume.gain.linearRampToValueAtTime(0, currentTime + offset + .5);

        setTimeout(function() {
            if (node.panner) {
                node.panner.disconnect();
            }
        }, (offset + 0.5) * 1000);

        delete node.handle.system.soundNodes.delete(node.id);
    }


    // soundNode
    // -----------------------------------------------------------------------
    //
    // Return the sound node belonging the the given sound component. Will
    // create the sound node if it doesn't already exist. The returned node
    // will have its 'updatedAt' timestamp set correctly to ensure it remains
    // active and is not garbage collected.

    function
    soundNode
    ( handle   : Handle
    , id       : string
    , url      : string
    , oneshot  : boolean
    , pos      : Vec3    // optional
    ): Node {
        // Disallow creating new sound nodes when the handle is currently
        // deactivated.
        if (!handle.system.handles.has(handle)) {
            return;
        }

        var node = handle.system.soundNodes.get(id);

        if (!node) {
            var buffer = bufferCache.get(url);
            if (buffer instanceof AudioBuffer) {
                var context      = handle.system.context
                  , bufferSource = context.createBufferSource()
                  , volume       = context.createGain()
                  , panner       = null;

                bufferSource.buffer = buffer;
                bufferSource.loop   = !oneshot;
                bufferSource.start(0);

                // The volume node is connected to the main output.
                volume.connect(handle.system.mainVolume);

                // If we have a position we connect the source through
                // a panner first and then to the volume node. Otherwise the
                // source goes straight to the volume.
                if (pos) {
                    panner = context.createPanner();
                    panner.setPosition(pos[0], pos[1], pos[2]);
                    bufferSource.connect(panner);
                    panner.connect(volume);
                } else {
                    bufferSource.connect(volume);
                }

                node = new Node
                    ( handle
                    , id
                    , State.Playing
                    , oneshot
                    , handle.generationNumber
                    , bufferSource
                    , volume
                    , pos
                    , panner
                    );

                handle.system.soundNodes.set(id, node);

                return node;

            } else {
                loadBuffer(handle.system.context, url);
            }

        } else {
            node.generationNumber = handle.generationNumber;
        }

        return node;
    }


    // loadBuffer
    // -----------------------------------------------------------------------
    //
    // Load a AudioBuffer from the given blob. Can be called repeatedly, the
    // function will send the GET request only once. The decoded buffer is
    // inserted into the buffer cache.

    function
    loadBuffer
    ( context : AudioContext
    , url     : string
    ): Promise<any> {
        var buf = bufferCache.get(url);

        if (buf instanceof Promise) {
            // The buffer is being loaded. Return the promise.
            return buf;

        } else if (buf) {
            // The buffer is already loaded. Return a promise which resolves
            // to the buffer.
            return Promise.resolve(buf);

        } else {
            // The buffer is not being loaded yet. Send the request and store
            // the promise in the buffer cache.
            var promise = new Promise((resolve, reject) => {
                var request = new XMLHttpRequest();

                request.open('GET', url, true);
                request.responseType = 'arraybuffer';

                var onSuccess = (buffer) => {
                    bufferCache.set(url, buffer);
                    resolve(buffer);
                };

                var onError = (err) => {
                    console.log("Could not load sound", err);
                    reject(err);
                };

                request.onload = () => {
                    context.decodeAudioData(request.response, onSuccess, onError);
                };

                request.send();
            });

            bufferCache.set(url, promise);

            return promise;
        }
    }



    export function
    relocateListener(handle: Handle, pos: Vec3): void {
        var listener = handle.system.context.listener;

        listener.setPosition(pos[0], pos[1], pos[2]);
        // TODO: setOrientation
    }
}
