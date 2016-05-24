import { AccountId } from '../../Pure/Ids';
import { Stage } from '../../Pure/Game';
import * as PG from '../../Pure/Game';
import { ITarget } from '../Server';
import * as Protocol from '../../Pure/Protocol';
import * as Shard from '../Server';
import { sendReply, handleTargetException, initiateTargetDestruction } from '../Server';
import { writePoint } from '../Metrics';
import { createGame, processMessage, nextEventAt } from '../../Game';
import config from '../../config';
import * as Message from '../../Pure/Message';
import * as J from '../../Pure/JSON';
import { State } from '../../Game/State';
import * as GS from '../../Game/State';
import { ContentProvider, createContentProvider } from '../../Game/ContentProvider';


var request = require('request');



// GameTargetState
//
//   = Loading
//     -- ^ The game state is being created (asynchronously). Incoming
//     -- messages are buffered in the queue.
//
//   | Running
//     -- ^ The game state is present, messages are being forwarded to it.
//     -- The tick timer is active.
//
//   | Deserted
//     -- ^ No client is connected to it anymore. The game is still running,
//     -- but it will be shut down unless a player joins within a minute. This
//     -- state is often encountered in local testing when the developer
//     -- reloads the client. Or at the end of the game when all players
//     -- leave.
//
//   | Aborted
//     -- ^ The game was aborted because something has thrown an exception.
//     -- The target is kept around for a while and then shut down. Incoming
//     -- messages are ignored, clients are immediately disconnected.


// The time after which a Deserted target is shut down.
var TIMEOUT = 60 * 1000;


export class GameTarget implements ITarget {

    epoch : number;
    // ^ The time when the game started. The in-game time is counted in
    // seconds since the epoch.

    tickTimeoutId : NodeJS.Timer;
    // ^ The timeout which will deliver the next TICK message. May be null
    // if the game does not have any Future scheduled.

    packetQueue : any[];
    // ^ The packet queue captures incoming messages while the game is
    // loading. Once the game is loaded the queue is destroyed and all
    // messages are forwarded to the game.

    state : State;
    // ^ The game state. It is created asynchronously. Only present when
    // the game is in Running or Deserted states.

    shutdownTimeoutId  : any;
    // ^ When the target is in Deserted or Aborted states, this is the
    // timer which will eventually shutdown the game.

    digestTimeoutId    : any;


    constructor
      ( public delegate : Shard.Server
      , public gameId   : string
      ) {
        this.epoch             = Date.now() / 1000;
        this.tickTimeoutId     = null;
        this.packetQueue       = [];
        this.shutdownTimeoutId = null;

        startDigestDaemon(this);
        prepareTarget(this);
    }


    // ITarget
    // -------------------------------------------------------------------

    error : Error = null;

    destructionTimeoutId : NodeJS.Timer = null;

    processMessage(accountId: AccountId, json): void {
        if (this.packetQueue) {
            this.packetQueue.push([currentTime(this), accountId, json]);

        } else if (this.state) {
            sendToSandbox(this, currentTime(this), accountId, json);

        } else {
            // The target is in Aborted state, ignore the message.
        }
    }
}



function
reportMessage(gameId: string): void {
    writePoint('shard.message', { value:true }, { target: 'game:' + gameId });
}



// prepareTarget
// -----------------------------------------------------------------------
//
// This function fetches the game content and creates the game state. At
// the end, the target transitions to Running (usually the case), Deserted
// (if all clients disconnected in the meantime) or Aborted (if there was
// an exception during loading of the content provider or initialization
// of the game state).

function
prepareTarget(target: GameTarget) {
    console.log('Creating content provider for game', target.gameId);
    createContentProvider(target.gameId, (err, contentProvider) => {
        if (contentProvider) {
            createState(target, contentProvider);

        } else {
            // Loading -> Aborted
            console.log("Failed to create content provider", err);
            handleException(target, err);
        }
    });
}


// createState
// -----------------------------------------------------------------------
//
// Given the ContentProvider, we can create the game state.

function
createState
( target          : GameTarget
, contentProvider : ContentProvider
): void {
    target.state = createGame(contentProvider);

    // createGame can fail if any of the initialization code (creating
    // parties, objectives, terrain instances etc) throws an error.

    if (target.state.error) {
        handleException(target, target.state.error);

    } else {

        // Before feeding messages into the state, update the stage in the
        // database (set to 'Setup') so that the API server accepts the
        // messages.

        completeInitialization(target, () => {
            var packetQueue = target.packetQueue;
            target.packetQueue = null;

            packetQueue.forEach((x) => {
                sendToSandbox(target, x[0], x[1], x[2]);
            });
        });
    }
}


// completeInitialization
// -----------------------------------------------------------------------
//
// At this point the game state has been successfully created and we can
// process all queued messages. But before we can do that we have to let
// the server know that we completed the initialization.

function
completeInitialization(target: GameTarget, fn): void {

    // Skip the lock when we don't have proper gameId for this game.
    if (config.encounterId) {
        fn();

    } else {
        var url = config.apiHost + "/games/" + target.gameId + "/digest";

        var body =
            { stage    : J.enumToJSON(Stage, Stage.Setup)
            , duration : 0
            , parties  : []
            };

        var options =
            { url  : url
            , body : JSON.stringify(body)
            };

        request.put(options, function(err, response, body) {
            if (response && response.statusCode === 200) {
                fn();

            } else if (err) {
                handleException(target, err);

            } else {
                handleException(target, new Error(
                    [ 'Failed to lock game'
                    , body
                    ].join('')
                ));
            }
        });
    }
}


// sendToSandbox
// -----------------------------------------------------------------------
//
// This function should only be used when the state is Running or
// Deserted.

function
sendToSandbox
( target    : GameTarget
, time      : number
, accountId : AccountId
, json      : any
) {
    var state = target.state;

    reportMessage(target.gameId);


    // TODO: Store the message in the database.
    // target.logger.log(time, accountId, opcode, json);

    processMessage(state, time, accountId, json);


    // Dispatch outgoing messages to clients.
    state.outgoingMessages.forEach(x => {
        x.set.forEach(accountId => {
            sendReply
                ( target.delegate
                , 'game'
                , target.gameId
                , accountId
                , x.msg
                , state.currentTime
                );
        });
    });
    state.outgoingMessages = [];


    if (!state.error) {
        // Schedule the next TICK message if needed.
        var nextEventTime = nextEventAt(state);
        if (nextEventTime) {
            scheduleUpdate(target, nextEventTime);
        }


        // Start or clear the shutdown timer if we transetion between
        // Running and Deserted.
        var isDeserted = PG.isDeserted(state);
        if (!target.shutdownTimeoutId && isDeserted) {
            // Running -> Deserted
            startShutdownTimer(target);

        } else if (target.shutdownTimeoutId && !isDeserted) {
            // Deserted -> Running
            clearShutdownTimer(target);
        }

    } else {
        throw state.error;
    }
}


function
startDigestDaemon(target: GameTarget) {
    if (!config.encounterId) {
        var oldStage = null;
        var url = config.apiHost + "/games/" + target.gameId + "/digest";

        target.digestTimeoutId = setTimeout(sendUpdate, 5000);

        function sendUpdate() {
            target.digestTimeoutId = setTimeout(sendUpdate, 5000);

            var state    = target.state
              , stage    = state ? state.stage : Stage.Setup
              , duration = state ? GS.duration(state) : 0;

            // Send the update only when we are past the Setup stage. This
            // ensures that all players have a valid roleId.
            if (oldStage !== stage && stage > Stage.Setup) {
                oldStage = stage;

                var parties = state.parties.map(party => {
                    var players = party.players.map(x => {
                        return { id     : x.accountId
                               , roleId : x.roleId
                               };
                    });

                    var objectives = party.objectives.map(x => {
                        return { id          : x.id
                               , isCompleted : x.isCompleted
                               };
                    });

                    return { id         : party.id
                           , score      : party.score
                           , players    : players
                           , objectives : objectives
                           };
                });

                var data =
                    { stage    : J.enumToJSON(Stage, stage)
                    , duration : duration
                    , parties  : parties
                    };

                var options =
                    { url  : url
                    , body : JSON.stringify(data)
                    };

                request.put(options, function(err, response, body) {
                    if (!err && response && response.statusCode === 200) {
                    } else {
                        console.log
                            ( 'Failed to sync game state to the API server'
                            , response
                            , err
                            , body
                            );
                    }
                });
            }
        }
    }
}

function
stopDigestDaemon(target: GameTarget): void {
    console.log("Stopping digest daemon for game", target.gameId);
    clearTimeout(target.digestTimeoutId);
}



// handleException
// -----------------------------------------------------------------------
//
// This is used to transition the target to the Aborted state.

function
handleException(target: GameTarget, e): void {
    handleTargetException(target.delegate, 'game', target.gameId, target, e);

    // Immediately shutdown the target. The actual destruction is handled
    // elsewhere (see handleTargetException).
    shutdown(target);
}


function
currentTime(target: GameTarget): number {
    return (Date.now() / 1000) - target.epoch;
}


function
scheduleUpdate(target: GameTarget, time: number): void {
    clearTimeout(target.tickTimeoutId);

    target.tickTimeoutId = setTimeout(() => {
        var msg  = new Message.TICK()
          , time = currentTime(target);

        try {
            sendToSandbox(target, time, null, Protocol.encodeMessage(msg, 0));
        } catch (e) {
            handleException(target, e);
        }

    }, (time - currentTime(target)) * 1000);
}


function
startShutdownTimer(target: GameTarget): void {
    if (!target.shutdownTimeoutId) {
        target.shutdownTimeoutId = setTimeout(() => {
            shutdown(target);
        }, TIMEOUT);
    }
}


function
shutdown(target: GameTarget): void {
    // Stop the clock.
    clearTimeout(target.tickTimeoutId);
    target.tickTimeoutId = null;

    initiateTargetDestruction(target.delegate, 'game', target.gameId);

    stopDigestDaemon(target);
}


function
clearShutdownTimer(target: GameTarget): void {
    clearTimeout(target.shutdownTimeoutId);
    target.shutdownTimeoutId = null;
}
