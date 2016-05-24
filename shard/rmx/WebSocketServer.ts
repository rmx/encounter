var express  = require('express');
var http     = require('http');
var ws       = require("websocket");

import { now } from './time';
import { TargetProxy } from './WebSocketProxy';
import * as Protocol from './Pure/Protocol';
import * as Message from './Pure/Message';




// The websocket connection as implemented by the 'websocket' package.
export interface WebSocketConnection {
    on(event: string, fn: Function): void;
    sendUTF(data: string): void;
}


function
mountWebSockets(app, delegate: WebSocketServer) {
    var options: any =
        { httpServer            : app
        , autoAcceptConnections : true
        };

    var websocketServer = new ws.server(options);
    websocketServer.on('connect', function(connection) {
        // XXX: tslint: expected an assignment or function call
        return new Connection(delegate, connection);
    });
}


export class Connection {

    account;
    targetProxy : TargetProxy;

    constructor
      ( public delegate   : WebSocketServer
      , public connection : WebSocketConnection
      ) {
        delegate.didOpenConnection(this);

        connection.on('message', (message) => {
            var json = JSON.parse(message.utf8Data);

            if (json.op === 'LATENCY') {
                var latency = now() - json.time;

                delegate.latencySamples.push(latency);
                if (delegate.latencySamples.length > 60 * 15) {
                    delegate.latencySamples.shift();
                }

            } else {
                delegate.didReceiveMessage(this, json);
            }
        });

        connection.on('close', () => {
            delegate.didCloseConnection(this);
        });
    }

    sendJSON(json): void {
        this.connection.sendUTF(JSON.stringify(json));
    }

    disconnectWith(reason: string): void {
        var msg = new Message.SMSG_DISCONNECT(reason);
        this.sendJSON(Protocol.encodeMessage(msg, 0));
    }
}



export interface IWebSocketServerDelegate {
    didOpenConnection(connection: Connection): void;
    didReceiveMessage(connection: Connection, json): void;
    didCloseConnection(connection: Connection): void;
}



export class WebSocketServer {

    app;
    connectionCount : number;

    watchdogConnection : any;
    // ^ If the watchdog is active, this is its connection.

    latencySamples : number[];
    // ^ The server keeps the last N latency samples. You can access them
    // directly or use one of the aggregation functions to generate
    // a more meaningful summary.


    constructor(private delegate: IWebSocketServerDelegate) {
        this.app = express();
        this.app.use(function(req, res) {
            (function(...args){ return args; })(req);
            res.status(404).send('Nothing to see here, move along.');
        });

        this.connectionCount    = 0;
        this.watchdogConnection = undefined;
        this.latencySamples     = [];
    }

    mountWebSocket(server) {
        mountWebSockets(server, this);
    }

    listen(port: number): void {
        var server = http.createServer(this.app).listen(port);
        this.mountWebSocket(server);

        createWatchdog(this, port);
    }

    close(fn: Function): void {
        this.app.close(fn);
    }

    didOpenConnection(connection: Connection): void {
        ++this.connectionCount;
        this.delegate.didOpenConnection(connection);
    }

    didReceiveMessage(connection: Connection, json): void {
        this.delegate.didReceiveMessage(connection, json);
    }

    didCloseConnection(connection: Connection): void {
        this.delegate.didCloseConnection(connection);
        --this.connectionCount;
    }
}


function
createWatchdog(server: WebSocketServer, port: number): void {
    console.log('Watchdog: creating a new one for port ' + port);

    var client = new ws.client();
    client.connect("ws://localhost:" + port);

    client.on('connectFailed', error => {
        console.log('Watchdog: connect failed', error);

        // Wait a bit and then try to create a new watchdog.
        setTimeout(() => {
            createWatchdog(server, port);
        }, 1000);
    });

    client.on('connect', connection => {
        server.watchdogConnection = connection;

        var intervalId = setInterval(() => {
            connection.sendUTF(JSON.stringify({ op: 'LATENCY', time: now() }));
        }, 1000);

        connection.on('error', error => {
            console.log('Watchdog: error', error);
            connection.close();
        });

        connection.on('close', () => {
            console.log('Watchdog: connection closed');

            clearInterval(intervalId);
            server.watchdogConnection = undefined;

            setTimeout(() => {
                createWatchdog(server, port);
            }, 1000);
        });

        connection.on('message', msg => {
            // The watchdog client should not receive any messages. But if
            // we do, log it.

            console.log('Watchog: received a message', msg);
        });
    });
}
