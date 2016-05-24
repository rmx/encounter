/// <reference path="../Pure/Ids.ts" />

/// <reference path="../Core/Bindings.ts" />
/// <reference path="../Core/Input.ts" />

/// <reference path="./State.ts" />
/// <reference path="./Types.ts" />
/// <reference path="./Actions.ts" />
/// <reference path="./Spell.ts" />

/// <reference path="./systems/renderer.ts" />
/// <reference path="./systems/terrain-position.ts" />
/// <reference path="./systems/move-path.ts" />
/// <reference path="./systems/audio.ts" />
/// <reference path="./systems/projectile.ts" />
/// <reference path="./systems/aura-tick.ts" />

/// <reference path="./decoders/client.ts" />
/// <reference path="./decoders/state.ts" />


// For accountAvatarUrl. Find a better place for that function.
/// <reference path="../Core/Account.ts" />
/// <reference path="../Core/Peer.ts" />


module rmx.Game {

    import AccountId              = rmx.Pure.AccountId;
    import EntityId               = rmx.Pure.EntityId;
    import SpellTarget            = rmx.Pure.SpellTarget;
    import lookupEntity           = rmx.Pure.lookupEntity;
    import clamp                  = rmx.Pure.clamp;
    import moveTerrainPosition    = rmx.Pure.moveTerrainPosition;

    import Bindings               = rmx.Core.Bindings;
    import Input                  = rmx.Core.Input;
    import KeyState               = rmx.Core.KeyState;


    export interface ISystemEntry {
        priority : number;
        system   : ISystem;
    }


    export class Stats {
        fps : FPS = new FPS;
    }

    export class FPS {

        value        : number = 0;
        startedAt    : number = null;
        frameCounter : number = 0;

        update(now: number): void {
            if (this.startedAt === null) {
                this.startedAt    = now;
                this.frameCounter = 0;

            } else if (this.startedAt + 1 < now) {
                this.value        = this.frameCounter / (now - this.startedAt);
                this.startedAt    = now;
                this.frameCounter = 0;

            } else {
                this.frameCounter += 1;
            }
        }
    }



    // Action
    // -------------------------------------------------------------------------
    //
    // Actions are the primary ways how a player interacts with the world (well,
    // other than walking around in it). We currently only have spells, so there
    // is only a SpellAction. In the future though we may add other types, such
    // as ItemAction (when we introduce inventory and items).
    //
    // See: useAction

    export type Action
        = SpellAction
        ;

    export class SpellAction {
        constructor
            ( public spellInfo : rmx.Pure.SpellEntry
            , public spellRef  : rmx.Storage.Reference
            ) {}
    }



    // The Client is used when a player is playing a game. It holds the common
    // game state as well as any state that is local to the player.

    export class Client {

        error    : Error;
        // ^ Initially null, only set when a fatal error occurs. See
        // 'fatalClientError'.

        stats    : Stats;
        // ^ The client collects various statistics about the performance,
        // health and general well-being of the client.

        state    : State;
        // ^ The game state.

        link     : Link;
        // ^ WebSocket connection to the server.

        input    : Input;
        // ^ The input state. All input events which have been captured during
        // the last frame, as well as any derived key/pointer state.

        bindings : Bindings<Client>;
        // ^ Input bindings.

        camera   : Camera;
        // ^ The location of the (primary) camera, relative to the controlled
        // entity.

        // Systems are ordered by priority. Those with lower priority are
        // run before those with higher priority.
        systems         : ISystemEntry[];

        lastUpdateAt    : number;

        updateRequestId : number;
        // ^ The requestId of the rAF callback. May be null if either the rAF
        // update loop hasn't been started yet or if the loop should be
        // stopped.


        // For the time being, we only support a single renderer per client.
        renderer            : rmx.Game.Systems.Renderer;

        createdAt           : number;
        // ^ The client time when the game was created.

        serverTime          : number;
        localTime           : number;

        // Local state
        // -------------------------------------------------------------------

        entityDidMove       : boolean;
        movementReportedAt  : number;

        desiredClassId : string = undefined;
        // ^ The character class which the user has selected on the client,
        // but the choice was not yet confirmed by the server.

        spells              : Map<string, rmx.Pure.SpellEntry>;
        actionButtons       : Action[];

        selectedAction      : Action;


        // Description of the local movement which is extracted from the input
        // state according to the currently active key bindings.
        localMovement : MovementDescription = {};

        showMovePaths : boolean = false;
        // ^ True if the renderer should show MovePaths. Useful for debugging.

        showNavmesh   : boolean = false;
        // ^ True if the navmesh should be rendered.

        hideChrome    : boolean = false;
        // ^ Whether to hide the chrome (useful for screenshots)

        audioHandle   : rmx.Core.Audio.Handle;


        constructor(public gameId: string) {
            this.error           = null;

            this.stats           = new Stats;
            this.state           = new State(gameId);
            this.link            = new Link(this);
            this.input           = new Input;
            this.bindings        = new Bindings(Actions);
            this.camera          = new Camera;

            this.systems         = [];
            this.update          = this.update.bind(this);

            this.lastUpdateAt    = null;
            this.updateRequestId = null;


            // Add essential systems to the state.
            // ---------------------------------------------------------------

            addSystem(this, 0, new rmx.Game.Systems.MovePath());
            addSystem(this, 1, new rmx.Game.Systems.TerrainPosition());
            addSystem(this, 2, new rmx.Game.Systems.Projectile());
            addSystem(this, 3, new rmx.Game.Systems.AuraTick(this));

            var audioSystem = new rmx.Game.Systems.Audio(this);
            this.audioHandle = audioSystem.handle;
            addSystem(this, 3, audioSystem);


            // Initialize the local state
            // ---------------------------------------------------------------

            this.entityDidMove      = false;
            this.movementReportedAt = null;

            this.spells             = new Map<string, rmx.Pure.SpellEntry>();
            this.actionButtons      = [];

            this.selectedAction     = null;


            // Create one renderer.
            this.renderer = new rmx.Game.Systems.Renderer(this);
            addSystem(this, 99, this.renderer);


            this.createdAt         = rmx.now();
            this.serverTime        = 0;
            this.localTime         = 0;

            loadDefaultBindings(this.bindings);
            loadAccountBindings(this.bindings, rmx.data.session.accountId);
        }


        // Smart accessors
        // -------------------------------------------------------------------

        get controlledEntityId(): EntityId {
            return rmx.Pure.playerControlledEntityId(this.state, rmx.data.session.accountId);
        }

        get controlledEntity(): WorldObject {
            return rmx.Pure.playerControlledEntity<WorldObject>(this.state, rmx.data.session.accountId);
        }

        get party(): rmx.Pure.Party {
            return rmx.Pure.playerParty(this.state, rmx.data.session.accountId);
        }


        // Client update callback
        // -------------------------------------------------------------------
        //
        // This is the main loop of the client. It is called regularly through
        // 'requestAnimationFrame'.

        update(timestamp: number): void {
            if (this.updateRequestId) {
                this.updateRequestId = requestAnimationFrame(this.update);
            }

            // V8 refuses to optimize functions which have a try/catch block.
            // So the bulk of the code is moved to a separate function and
            // here we just wrap it in a tiny try/catch block.

            try {
                updateClient(this, timestamp);
            } catch (e) {
                fatalClientError(this, e);
            }
        }
    }


    function
    updateClient(client: Client, timestamp: number): void {
        var now = timestamp / 1000
          , dt  = now - client.lastUpdateAt;

        client.lastUpdateAt = now;
        client.stats.fps.update(now);

        rmx.Core.dispatchInput(timestamp, client.input, client.bindings, client);

        applyLocalMovement(client, now, dt);
        client.localMovement = {};

        client.systems.forEach(systemEntry => {
            systemEntry.system.update(client.state, now, dt);
        });


        // Attempt to open a peer connection to all players in this game.
        client.state.parties.forEach(party => {
            party.players.forEach(player => {
                if (player.accountId !== rmx.data.session.accountId && !rmx.Core.Peer.hasConnection(rmx.app.peerH, player.accountId)) {
                    openPeerConnection(rmx.app.peerH, player.accountId);
                }
            });
        });


        // Select the first available class if the player hasn't done so.
        if (client.desiredClassId === undefined) {

            // FIXME: There is a tiny (and unrealistic) race condition!
            // We should pick a class from the party where the player is. But we
            // know that players join the first party and by the time this code
            // is running the player didn't yet have a chance to change the
            // party. So...

            var classId = rmx.data.objectContent<rmx.Storage.Game>(client.state.gameId).bind(game => {
                return rmx.data.objectContent<rmx.Storage.Encounter>(game.encounter.objectId).fmap(encounter => {
                    return encounter.parties[0].classes[0].toString();
                });
            }).get(undefined);

            if (classId !== undefined) {
                selectRole(client, classId);
            }
        }
    }

    function openPeerConnection(h: rmx.Core.Peer.Handle, objId: rmx.Pure.AccountId): void {
        var stream = rmx.app.userMediaStream().get(undefined);
        if (stream) {
            rmx.Core.Peer.openConnection(h, stream, objId);
        }
    }


    // MovementDescription
    // -----------------------------------------------------------------------
    //
    // Description how the local player wants to move. The fields are set by
    // the bindings, which are driven by the input state.

    export interface MovementDescription {
        forward     ?: KeyState;
        backward    ?: KeyState;

        strafeLeft  ?: KeyState;
        strafeRight ?: KeyState;

        lockHeading ?: boolean;
        // ^ True if the player's heading should be locked to the camera.
    }

    function forwardMovement(md: MovementDescription): number {
        if (md.forward) {
            return +6.4;
        } else if (md.backward) {
            return -2.2;
        } else {
            return 0;
        }
    }

    function strafeMovement(md: MovementDescription): number {
        if (md.strafeLeft) {
            return -4;
        } else if (md.strafeRight) {
            return 4;
        } else {
            return 0;
        }
    }

    function
    computeLocalMovement
    ( md     : MovementDescription
    , entity : WorldObject
    ): { x: number; y: number; } {
        // FIXME: Use the KeyState totalTime to scale the movement.
        // TODO: Rotation along the Z axis;

        // XXX: Why divide by 3?
        var speed = entity.movementSpeed / 3;

        var yv = forwardMovement(md) * speed;
        var xv = strafeMovement(md)  * speed;

        return { x: xv, y: yv };
    }



    // fatalClientError
    // -----------------------------------------------------------------------
    //
    // This can be called by anybody to report a (fatal) client error. The
    // rendering loop stops and the UI will show the error.
    //
    // Only the first error is stored in the client. Any future errors
    // are ignored.
    //
    // Developers can inspect the error through the developer console:
    //
    //   > rmx.data.activeGame.error

    export function
    fatalClientError(client: Client, error: Error): void {
        if (client.error === null) {
            client.error = error;
            rmx.data.startNextGeneration();
        }
    }


    // triggerClick
    // -----------------------------------------------------------------------
    //
    // Used by bindings to indicate that the user has pressed the primary
    // pointer with the intention to select the entity under the mouse or use
    // the action at the position where the primary pointer is currently
    // located.

    export function
    triggerClick(client: Client): void {
        /// XXX: tslint: duplicate variable
        var intersect;

        if (client.selectedAction) {
            // The client is waiting for user input where to use the selected
            // action at/on.

            // The intersection of a tile under the pointer.
            intersect = WebGL.intersectionAtPointer(client.renderer.scene, obj => {
                var owner = WebGL.toRenderObject(obj);
                return owner && !!owner.id.match(/\/ti$/);
            });

            if (intersect) {
                var point       = intersect.point
                  , position    = vec3.fromValues(point.x, point.y, point.z)
                  , spellId     = client.selectedAction.spellInfo.id
                  , spellTarget = new SpellTarget(null, <any>position);

                console.log("User selected a location to use the action at", vec3.str(position));

                sendSpellcastMessage(client, spellId, spellTarget);
                client.selectedAction = null;
            }

        } else {
            // A WorldObject under the pointer.
            intersect = WebGL.intersectionAtPointer(client.renderer.scene, obj => {
                var owner = WebGL.toRenderObject(obj);
                return owner && (!owner.id.match(/\/ti$/) && !owner.id.match(/^skybox/));
            });

            if (intersect) {
                selectEntity(client, intersect.owner.id);
            }
        }
    }


    // The link provides a persistent, full-duplex channel between the client
    // and the server. It automatically synchronizes the game state with
    // the server.
    //
    // In case the client is interactive, it can also be used to send messages
    // to the server.

    export class Link {

        private client : rmx.Game.Client;
        private socket : WebSocket;


        constructor(client: rmx.Game.Client) {
            this.client = client;
            this.socket = new WebSocket(rmx.config.wsHost);

            this.socket.addEventListener('open',    this.open.bind(this));
            this.socket.addEventListener('message', this.message.bind(this));
            this.socket.addEventListener('close',   this.close.bind(this));
            this.socket.addEventListener('error',   this.error.bind(this));
        }

        get ready(): boolean {
            return this.socket.readyState === this.socket.OPEN;
        }

        sendMessage(msg: { op: number; content: any; }): void {
            try {
                if (this.socket.readyState === this.socket.OPEN) {
                    var data = JSON.stringify(rmx.Protocol.encodeMessage(msg, 0));
                    this.socket.send(data);

                } else {
                    throw new Error('WebSocket connection is not open');
                }

            } catch (e) {
                fatalClientError(this.client, e);
            }
        }

        private open(ev: Event): void {
            console.log('Link opened');

            // Kick off the rAF update loop on the client.
            if (this.client.updateRequestId == null) {
                this.client.lastUpdateAt    = window.performance.now() / 1000;
                this.client.updateRequestId = requestAnimationFrame(this.client.update);
            }

            var token = <string>rmx.data.session.accountId
              , bind  = { type: 'game', id: this.client.state.gameId }
              , msg   = new rmx.Pure.Message.CMSG_AUTHENTICATE(token, bind);

            this.sendMessage(msg);
        }

        private message(ev: any): void {
            try {
                var msg    = JSON.parse(ev.data)
                  , op     = msg.opcode
                  , opName = rmx.Protocol.opName(op);

                if (!opName) {
                    throw new Error(
                        [ 'Unknown opcode in message: '
                        , op
                        ].join('')
                    );
                }

                this.client.serverTime = msg.time;
                this.client.localTime  = rmx.now();


                // This was here for debugging only, but causes too much
                // overhead in long running games.
                //
                // console.log('Link message', opName);

                if (rmx.Game.Decoder.Client[opName]) {
                    rmx.Game.Decoder.Client[opName](this.client, msg, this);
                } else if (rmx.Game.Decoder.State[opName]) {
                    rmx.Game.Decoder.State[opName](this.client.state, msg, this);
                } else {
                    throw new Error(
                        [ 'Unhandled opcode: '
                        , opName
                        ].join('')
                    );
                }

            } catch (e) {
                fatalClientError(this.client, e);
            }


            // If the player has already selected a role, reflect that in the
            // client.
            var player = rmx.Pure.lookupPlayer(this.client.state, rmx.data.session.accountId);
            if (player && player.roleId != undefined) {
               this.client.desiredClassId = player.roleId;
            }


            rmx.data.startNextGeneration();
        }

        private close(ev: Event): void {
            console.log('Link closed');

            cancelAnimationFrame(this.client.updateRequestId);
            this.client.updateRequestId = null;

            rmx.data.startNextGeneration();
        }

        private error(ev: Event): void {
            var error = new Error("Link error: " + JSON.stringify(ev));
            fatalClientError(this.client, error);
        }
    }



    // Camera
    // -----------------------------------------------------------------------
    //
    // Stores the position of a camera relative to its target.

    export class Camera {

        pitch       : number;
        distance    : number;
        heading     : number;

        minDistance : number;
        maxDistance : number;


        constructor() {
            this.pitch       = Math.PI / 3;
            this.distance    = 20;
            this.heading     = 0;

            this.minDistance = 2.5;
            this.maxDistance = 30;
        }


        // The camera's position relative to the given target.
        positionRelativeTo(target: Vec3): Vec3 {
            var h = this.heading;

            // The offset from the target position.
            var ox = 0
              , oy = - this.distance * 1 * Math.cos(this.pitch)
              , oz =   this.distance * 1;

            return vec3.fromValues
                ( target[0] + Math.sin(h) * ox + Math.cos(h) * oy
                , target[1] + Math.cos(h) * ox + Math.sin(h) * oy
                , target[2] + Math.sin(this.pitch) * oz
                );
        }


        changeHeading(diff: number): void {
            this.heading -= diff;
        }

        changePitch(diff: number): void {
            this.pitch = clamp(0.1, Math.PI / 2, this.pitch + diff);
        }

        changeZoom(diff: number): void {
            this.distance = clamp(this.minDistance, this.maxDistance, this.distance - diff);
        }
    }


    function
    loadDefaultBindings(bindings: Bindings<any>): void {
        bindings.bindings['w']           = { action: 'move-forward'         };
        bindings.bindings['s']           = { action: 'move-backward'        };
        bindings.bindings['a']           = { action: 'strafe-left'          };
        bindings.bindings['d']           = { action: 'strafe-right'         };

        bindings.bindings['1']           = { action: 'action-button-1'      };
        bindings.bindings['2']           = { action: 'action-button-2'      };
        bindings.bindings['3']           = { action: 'action-button-3'      };
        bindings.bindings['4']           = { action: 'action-button-4'      };
        bindings.bindings['5']           = { action: 'action-button-5'      };
        bindings.bindings['6']           = { action: 'action-button-6'      };
        bindings.bindings['7']           = { action: 'action-button-7'      };
        bindings.bindings['8']           = { action: 'action-button-8'      };
        bindings.bindings['9']           = { action: 'action-button-9'      };

        bindings.bindings['esc']         = { action: 'cancel-action'        };
        bindings.bindings['tab']         = { action: 'target-nearest-enemy' };

        bindings.bindings['printscreen'] = { action: 'screenshot'           };

        bindings.bindings['wheelup']     = { action: 'camera-zoom-in'       };
        bindings.bindings['wheeldown']   = { action: 'camera-zoom-out'      };

        bindings.bindings['button-1']    = { action: 'select'               };
        bindings.bindings['button-3']    = { action: 'turn-or-action'       };


        bindings.bindings['[']           = { action: 'toggle-navmesh-rendering'  };
        bindings.bindings[']']           = { action: 'toggle-movepath-rendering' };
        bindings.bindings['ctrl+h']      = { action: 'toggle-chrome' };
    }

    function
    loadAccountBindings(bindings: Bindings<any>, accountId: AccountId): void {
        var obj = rmx.data.loadById<rmx.Storage.Account>(<string>accountId);
        obj.promise.then(account => {
            if (account.content && account.content.bindings) {
                account.content.bindings.bindings.forEach(binding => {
                    binding.triggers.forEach(trigger => {
                        bindings.bindings[trigger] = { action: binding.action };
                    });
                });

            } else {
                console.warn("Failed to load the account.");
            }
        });
    }


    function
    useSpellAction
    ( client : Client
    , action : SpellAction
    ): void {
        var spellId = action.spellInfo.id
          , entity  = client.controlledEntity
          , spell   = rmx.Core.resourceContentReference<rmx.Storage.Spell>(action.spellRef).get(null);

        // XXX: tslint: duplicate variable
        var spellTarget;

        if (entity && spell) {
            if (spell.targetType === 'self') {
                spellTarget = new SpellTarget(entity.id, null);
                sendSpellcastMessage(client, spellId, spellTarget);

            } else if (spell.targetType === 'world-object' && entity.targetId) {
                spellTarget = new SpellTarget(entity.targetId, null);
                sendSpellcastMessage(client, spellId, spellTarget);

            } else if (spell.targetType === 'location') {
                console.log('Spell target type is location, waiting for user to select one');
                client.selectedAction = action;

            } else {
                console.log('Unknown spell target type:', spell.targetType);
            }
        }
    }


    function
    sendSpellcastMessage
    ( client      : Client
    , spellId     : string
    , spellTarget : SpellTarget
    ): void {
        var msg = new rmx.Pure.Message.CMSG_SPELLCAST(spellId, spellTarget);
        client.link.sendMessage(msg);
    }


    export function
    invitePlayer(client: Client, accountId: string): void {
        client.link.sendMessage(new rmx.Pure.Message.CMSG_INVITE(accountId));
    }


    // selectEntity
    // -----------------------------------------------------------------------
    //
    // Called when the player selects an entity, for examply by clicking
    // on its model inside the canvas, or selecting it through one of the
    // unit frames.

    export function
    selectEntity(client: Client, entityId: EntityId): void {
        var entity = client.controlledEntity;
        if (entity) {
            entity.targetId = entityId;
            client.link.sendMessage(new rmx.Pure.Message.CMSG_TARGET(entityId));
        }
    }


    export function
    sendChatMessage(client: Client, text: string): void {
        var msg = new rmx.Pure.Message.CMSG_CHATMESSAGE(text);
        client.link.sendMessage(msg);
    }


    export function
    useActionButton(client: Client, index: number): void {
        var action = client.actionButtons[index];
        if (action) {
            useAction(client, action);
        }
    }



    // Not used anywhere. Commented out so I can get rid of Link#send.
    // kickPlayer(accountId: string): void {
    //     this.link.send({ opcode: rmx.Protocol.op.CMSG_KICK, accountID: accountId });
    // }

    export function
    toggleReady(client: Client): void {
        var msg = new rmx.Pure.Message.CMSG_CONFIRM_CHARACTER_CHOICE();
        client.link.sendMessage(msg);
    }


    export function
    selectRole(client: Client, classId: string): void {
        if (client.desiredClassId !== classId) {
            client.desiredClassId = classId;
            var msg = new rmx.Pure.Message.CMSG_CHANGE_CHARACTER(classId);
            client.link.sendMessage(msg);
        }
    }


    export function
    changeParty(client: Client, partyId: string): void {
        var msg = new rmx.Pure.Message.CMSG_CHANGE_PARTY(partyId);
        client.link.sendMessage(msg);
    }


    export function
    useAction(client: Client, action: Action): void {
        if (action instanceof SpellAction) {
            return useSpellAction(client, action);

        } else {
            console.log('Unknown action type', action);
        }
    }


    export function
    actionCooldownLeft(client: Client, action: Action): number {
        var entity = client.controlledEntity;
        if (entity) {
            if (action instanceof SpellAction) {
                return spellCooldownLeft(client, entity, action.spellInfo.id);

            } else {
                console.log('Unknown action type', action);
            }
        }
    }


    export function
    takeScreenshot(client: Client): void {
        // FIXME: No type safety here, we're accesssing internal properties
        // of the WebGL Scene.
        var canvas = <any> client.renderer.scene.domElement;

        canvas.toBlob(blob => {
            rmx.data.createImage(blob, client.state.gameId).then(imageId => {
                console.log('Created image', imageId);
            });
        });
    }


    export function
    targetNearestEnemy(client: Client): void {
        if (!client.controlledEntity) {
            return;
        }

        var distances  = {};

        // FIXME: Have a better way to enumerate all entity Ids.
        // Use `client.state.entities.keys()` once typescript has the updated
        // definition of the Map<K,V> type.
        var entityIds = [];
        client.state.entities.forEach((entity, entityId) => {
            entityIds.push(entityId);
        });

        var candidates = entityIds.filter(entityId => {
            var entity = lookupEntity(client.state, entityId);

            if (entityId == client.controlledEntityId) {
                return false;

            } else if (entity instanceof WorldObject) {

                var ent = <WorldObject>entity;
                var pla = <WorldObject>client.controlledEntity;

                // Skip dead entities.
                if (ent.health === 0) {
                    return false;
                }

                // If we're unlucky, the position is not available yet.
                if (!ent.position) {
                    return false;
                }

                // check if entity is in the same terrain
                if(ent.terrainPosition.terrainId !=
                   pla.terrainPosition.terrainId)
                    return false;

                // check if the entity is in a circle around the player
                var dx = ent.position[0] - pla.position[0]
                  , dy = ent.position[1] - pla.position[1];
                distances[entityId] = dx * dx + dy * dy;
                if(dx * dx + dy * dy <= 4.0)
                    return true;

                // then check if the entity is in a triangle with an
                // opening angle of 60 degree.
                var theta = pla.terrainPosition.heading;
                var opening = Math.PI / 6.0;

                var p  = ent.position;
                var p0 = pla.position;
                var p1 = [50 * Math.cos(theta - opening),
                          50 * Math.sin(theta - opening)];
                var p2 = [50 * Math.cos(theta + opening),
                          50 * Math.sin(theta + opening)];
                var s = (p0[1] * p2[0] - p0[0] * p2[1] +
                        (p2[1] - p0[1]) * p[0] + (p0[0] - p2[0]) * p[1]);
                var t = (p0[0] * p1[1] - p0[1] * p1[0] +
                        (p0[1] - p1[1]) * p[0] + (p1[0] - p0[0]) * p[1]);

                if (s <= 0 || t <= 0)
                    return false;

                var area = (-p1[1] * p2[0] + p0[1] * (-p1[0] + p2[0]) +
                            p0[0] * (p1[1] - p2[1]) + p1[0] * p2[1]);

                return (s + t) < area;
            }

            return false;
        });

        if (candidates.length > 0) {
            candidates.sort((a, b) => {
                return (distances[a] - distances[b]);
            });

            var targetId = client.controlledEntity.targetId
              , oldIndex = candidates.indexOf(<string> targetId)
              , newIndex = (oldIndex + 1) % candidates.length
              , entityId = candidates[newIndex];

            selectEntity(client, entityId);
        }
    }


    // applyLocalMovement
    // -----------------------------------------------------------------------
    //
    // Move the controlled entity according to the pressed keys.

    function
    applyLocalMovement(client: Client, now: number, dt: number): void {
        var entity = client.controlledEntity;

        if (entity && !entity.stunned && entity.soul === 'present') {
            var movement = computeLocalMovement(client.localMovement, entity)
              , xv       = movement.x
              , yv       = movement.y;

            if (xv !== 0 || yv !== 0) {
                var position = entity.terrainPosition
                  , terrain  = lookupEntity<rmx.Pure.Terrain>(client.state, position.terrainId)
                  , heading  = position.heading
                  , dx       = dt * ( xv * Math.sin(heading) + yv * Math.cos(heading))
                  , dy       = dt * (-xv * Math.cos(heading) + yv * Math.sin(heading));

                moveTerrainPosition(position, terrain, vec3.fromValues(dx, dy, 0));
                entity.moveFlags = 1;

                client.entityDidMove = true;
            } else {
                entity.moveFlags = null;
            }

            if (client.localMovement.lockHeading) {
                entity.terrainPosition.heading = client.camera.heading;
            }
        }


        if (client.entityDidMove && client.movementReportedAt < now - 0.2) {
            client.entityDidMove      = false;
            client.movementReportedAt = now;

            client.link.sendMessage(new rmx.Pure.Message.CMSG_MOVE
                ( entity.terrainPosition
                , entity.moveFlags
                )
            );
        }
    }


    function
    addSystem(client: Client, priority: number, system: ISystem): void {
        client.systems.push({ priority: priority, system: system });
        client.systems.sort((a, b) => { return a.priority - b.priority; });
    }


    export function
    portraitUrl(client: Client, entity: rmx.Pure.IEntity): string {
        var player = rmx.Pure.controllingPlayer(client.state, entity);

        if (player) {
            return rmx.Core.accountAvatarUrl(player.accountId);

        } else {
            return rmx.assets.creatureAvatarImage;
        }
    }


    export function
    targetEntity(client: Client, entity: WorldObject): WorldObject {
        if (entity) {
            var ret = rmx.Pure.lookupEntity<WorldObject>(client.state, entity.targetId);
            if (ret && ret instanceof WorldObject) {
                return ret;
            }
        }
    }

    export function
    targetOfTargetEntity(client: Client, entity: WorldObject): WorldObject {
        var target = targetEntity(client, entity);
        if (target) {
            var ret = rmx.Pure.lookupEntity<WorldObject>(client.state, target.targetId);
            if (ret && ret instanceof WorldObject) {
                return ret;
            }
        }
    }
}
