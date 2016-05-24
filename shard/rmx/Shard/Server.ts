/// <reference path="../../lib/node.d.ts" />

var stackTrace = require('stack-trace');

import { AccountId } from '../Pure/Ids';
import * as Protocol from '../Pure/Protocol';
import * as Message from '../Pure/Message';
import * as GM from '../Game/Messages';

import { writePoint } from './Metrics';
import { GameTarget } from './Target/Game';

import { WebSocketServer, Connection, IWebSocketServerDelegate } from '../WebSocketServer';

var targetDestructionDelay = 20 * 60 * 1000; // 20 minutes
// ^ For how long targets are kept around before they are destroyed.


export interface ITarget {

    error : Error;
    // ^ When present, messages to this target will be automatically
    // rejected and answered with a DISCONNECT. See 'handleTargetException'.

    destructionTimeoutId : NodeJS.Timer;
    // ^ If destruction was initiated (either due to an exception
    // or through normal lifecycle), then this will hold the timer
    // which will fire when the target needs to be destroyed.
    //
    // The destruction is final and can not be stopped. Incoming messages
    // are rejected and answered with DISCONNECT.

    processMessage(accountId: AccountId, json): void;
    // ^ Handler for incoming messages. May throw exceptions, they are
    // treated as fatal errors which cause the target to be destroyed.

}



// Metrics
// -----------------------------------------------------------------------
//
// These are the metrics we keep for the whole server. These are
// periodically flushed to some backend (when configured).

export class Metrics {

    packetsRx : number = 0;
    // ^ Number of packets received from the proxy;

    packetsTx : number = 0;
    // ^ Number of packets sent to the proxy.

}


function
flushMetrics(server: Server): void {
    writePoint('shard.packet.rx.count',
        { value : server.metrics.packetsRx },
        { shard : server.shardId }
    );

    writePoint('shard.packet.tx.count',
        { value : server.metrics.packetsTx },
        { shard : server.shardId }
    );


    // Reset the metrics. I'm lazy so I reallocate the whole class.
    server.metrics = new Metrics;
}



// Server
// -----------------------------------------------------------------------

export class Server implements IWebSocketServerDelegate {

    webSocketServer : WebSocketServer;
    // ^ The WebSocket server listens on the internal port and accepts
    // connections from the proxy.

    targets : Map<string, ITarget>;
    // ^ The targets (games) which are currently loaded.

    clientConnectionMap  : { [accountId: string]: Connection };
    clientTargetMap      : { [accountId: string]: ITarget };

    startedShutdown      : boolean;

    metrics = new Metrics;
    // ^ Server metrics.


    constructor
      ( public shardId : string
      , internalPort   : number
      ) {
        this.webSocketServer = new WebSocketServer(this);
        this.webSocketServer.listen(internalPort);

        this.targets             = new Map<string, ITarget>();
        this.clientConnectionMap = Object.create(null);
        this.clientTargetMap     = Object.create(null);

        setInterval(() => {
            flushMetrics(this);
        }, 1000);
    }


    // External API
    // -------------------------------------------------------------------

    gracefulShutdown(fn): void {
        var numClients = Object.keys(this.clientConnectionMap).length;
        console.log("" + numClients + " clients still connected...");

        if (!this.startedShutdown) {
            this.startedShutdown = true;
            this.webSocketServer.close(fn);
        }
    }



    // WebSocketServer delegate
    // -------------------------------------------------------------------

    didOpenConnection() {
        console.log("rmx.Shard.Server: new connection...");
    }

    didReceiveMessage(connection: Connection, json) {
        this.metrics.packetsRx += 1;

        // XXX: We assume that the incoming packet is well-formed.

        var accountId = json.accountID
          , type      = json.target.type
          , id        = json.target.id
          , msg       = json.msg;

        // Immediately register the connection in the client connection
        // map. This overrides any previous associations between accountId
        // and incoming connection.
        this.clientConnectionMap[accountId] = connection;

        var target = getTarget(this, type, id);
        if (target) {
            this.clientTargetMap[accountId] = target;
            processTargetMessage(this, accountId, type, id, target, msg);

        } else {
            var rmsg = new Message.SMSG_DISCONNECT('Could not find target');
            sendReply(this, type, id, accountId, rmsg, 0);
        }
    }


    // Called when the other side (the websocket proxy) closes the
    // connection.
    //
    // We send SMSG_DISCONNECT on behalf of all clients which were
    // connected to targets through that connection.

    didCloseConnection(connection: Connection) {
        console.log("rmx.Shard.Server: connection closed");

        Object.keys(this.clientConnectionMap).forEach(accountId => {
            if (this.clientConnectionMap[accountId] === connection) {
                delete this.clientConnectionMap[accountId];

                var target = this.clientTargetMap[accountId];
                if (target) {
                    var msg = new Message.SMSG_DISCONNECT('client disconnected');
                    target.processMessage(accountId, Protocol.encodeMessage(msg, 0));
                }
            }
        });
    }
}



// initiateTargetDestruction
// -----------------------------------------------------------------------
//
// This action is final, there is no way to undo the destruction. When you
// call this function, you MUST be certain that the target can go.
//
// All clients will be disconnected, no more connections to that target
// will be allowed. New conections will be answered with a DISCONNECT.

export function
initiateTargetDestruction(server: Server, type: string, id: string): void {
    var key    = type + "/" + id
      , target = server.targets.get(key);

    if (target && target.destructionTimeoutId === null) {
        console.log('initiateTargetDestruction', type, id);


        // Start the destruction timer. We keep the target around in case
        // a client tries to connect to it.

        target.destructionTimeoutId = setTimeout(() => {
            server.targets.delete(key);
        }, targetDestructionDelay);


        // Disconnect all clients.

        for (var accountId in server.clientTargetMap) {
            if (server.clientTargetMap[accountId] === target) {
                sendDisconnect(server, accountId, type, id, target);
                disconnectAccount(server, accountId);
            }
        }
    }
}



// sendDisconnect
// -----------------------------------------------------------------------
//
// Send a DISCONNECT message to the client, and include the correct
// reason.

function
sendDisconnect
( server     : Server
, accountId  : AccountId
, type       : string
, id         : string
, target     : ITarget
): void {
    var reply;

    if (target.error) {
        reply = new Message.SMSG_DISCONNECT(target.error.message);
        sendReply(server, type, id, accountId, reply, 0);

    } else if (target.destructionTimeoutId) {
        reply = new Message.SMSG_DISCONNECT('Target is being destroyed');
        sendReply(server, type, id, accountId, reply, 0);

    } else {
        reply = new Message.SMSG_DISCONNECT('Bye');
        sendReply(server, type, id, accountId, reply, 0);
    }
}


// handleTargetException
// -----------------------------------------------------------------------
//
// Can be called repeatedly with the same target, but only the first has
// any effect. Subsequent calls will be silently ignored to preserve the
// first exception which caused the abort.

export function
handleTargetException
( server : Server
, type   : string
, id     : string
, target : ITarget
, e
): void {
    if (target.error) {
        return;
    }


    target.error = e;
    initiateTargetDestruction(server, type, id);


    console.log('handleTargetException');
    console.log(e);


    // Parsing the stack trace may fail. Print a log message and
    // ignore the exception.
    try {
        stackTrace.parse(e).forEach(cs => {
            console.log(' -> ', cs.getFunctionName());
        });
    } catch (e) {
        console.log('Exception while printing stack trace', e.message);
    }


    if (process.env.EXCEPTIONS === "throw") {
        throw e;
    }


    // Send SMSG_DISCONNECT to all accounts connected to the target.

    for (var accountId in server.clientTargetMap) {
        if (server.clientTargetMap[accountId] === target) {
            sendDisconnect(server, accountId, type, id, target);
            disconnectAccount(server, accountId);
        }
    }
}



function
processTargetMessage
( server     : Server
, accountId  : AccountId
, type       : string
, id         : string
, target     : ITarget
, msg        : any
): void {
    if (target.error || target.destructionTimeoutId) {
        sendDisconnect(server, accountId, type, id, target);

    } else {
        try {
            target.processMessage(accountId, msg);
        } catch (e) {
            handleTargetException(server, type, id, target, e);
        }
    }
}


export function
sendReply
( server    : Server
, type      : string
, id        : string
, accountId : any
, message   : GM.Message
, time      : number
): void {
    var connection = server.clientConnectionMap[accountId];
    if (connection) {
        var payload =
            { target    : { type: type, id: id }
            , accountID : accountId
            , msg       : Protocol.encodeMessage(message, time)
            };

        try {
            connection.sendJSON(payload);

            // Only increment the tx packets metric on when we
            // successfully send the packet out.
            server.metrics.packetsTx += 1;

        } catch (e) {
            disconnectAccount(server, accountId);
        }
    }
}



// getTarget
// -----------------------------------------------------------------------
//
// Note: Throws an error if the target type is unknown.

function
getTarget(server: Server, type: string, id: string): ITarget {
    var key    = type + "/" + id
      , target = server.targets.get(key);

    if (!target) {
        target = createTarget();
        server.targets.set(key, target);
    }

    return target;

    function createTarget(): ITarget {
        console.log("Creating target " + type + "/" + id);

        switch (type) {
        case 'game': return new GameTarget(server, id);
        }
    }
}


function
disconnectAccount(server: Server, accountId): void {
    delete server.clientConnectionMap[accountId];
    delete server.clientTargetMap[accountId];
}
