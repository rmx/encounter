import { AccountId } from './Pure/Ids';
import * as Protocol from './Pure/Protocol';
import { WebSocketServer, Connection } from './WebSocketServer';
import * as Message from './Pure/Message';

let WebSocketClient = require('websocket').client;


export class WebSocketProxy {

    webSocketServer     : WebSocketServer;
    clientConnectionMap : { [key: string]: Connection };
    containerConnection : ContainerConnection;

    constructor() {
        this.webSocketServer     = new WebSocketServer(this);
        this.clientConnectionMap = {};
        this.containerConnection = null;
    }

    httpServer() {
        return this.webSocketServer;
    }

    didOpenConnection() {
        console.log('WebSocketProxy: A client opened a new connection');
    }

    didReceiveMessage(connection: Connection, json) {
        if (!connection.account) {
            return this.forceAuthentication(connection, json);
        }

        var target = connection.targetProxy;
        if (target) {
            return target.forwardMessage(json);
        }

        connection.disconnectWith('Client error (no bind target selected)');
    }

    didCloseConnection(connection: Connection) {
        if (connection.account) {
            delete this.clientConnectionMap[connection.account];
            var target = connection.targetProxy;
            if (target) {
                target.disconnect();
            }
        }
    }

    forceAuthentication(connection: Connection, json) {
        if (json.opcode === Protocol.op.CMSG_AUTHENTICATE) {
            connection.account = json.token;

            var msg = new Message.SMSG_PROFILE(json.token, json.token);
            connection.sendJSON(Protocol.encodeMessage(msg, 0));

            this.clientConnectionMap[json.token] = connection;
            this.setTargetProxy(connection, json.token, json.bind.type, json.bind.id);
        } else {
            connection.disconnectWith('Client error (authentication expected)');
        }
    }

    setTargetProxy(connection: Connection, accountId, type: string, id: string): void {
        if (!this.containerConnection) {
            this.containerConnection = new ContainerConnection(this, 'localhost', 3010);
        }

        connection.targetProxy = new TargetProxy
            ( connection
            , this.containerConnection
            , accountId
            , type
            , id);
    }

    replyPacket(spec, accountId, json) {
        var connection = this.clientConnectionMap[accountId];
        if (connection) {
            var tp = connection.targetProxy;
            if (tp && tp.matchesTargetSpec(spec)) {
                connection.sendJSON(json);
            }
        }
    }

    proxyShutdown() {
        if (this.containerConnection) {
            console.log("Connection to shard was disconnected");

            var containerConnection = this.containerConnection;
            this.containerConnection = null;

            containerConnection.close();
        }
    }
}



export class TargetProxy {

    private target : { type: string; id: string; };

    constructor
    ( public connection          : Connection
    , public containerConnection : ContainerConnection
    , public accountId           : AccountId
    , public type                : string
    , public id                  : string
    ) {
        this.target = { type: type, id: id };
        this.containerConnection.registerConnection(connection);

        var msg = new Message.CONNECT();
        this.forwardMessage(Protocol.encodeMessage(msg, 0));
    }


    forwardMessage(json): void {
        var payload =
            { accountID : this.accountId
            , target    : this.target
            , msg       : json
            };

        this.containerConnection.sendPacket(payload);
    }

    disconnect(): void {
        var msg = new Message.SMSG_DISCONNECT('...');
        this.forwardMessage(Protocol.encodeMessage(msg, 0));
        this.containerConnection.clearConnection(this.connection);
    }

    matchesTargetSpec(spec: { type: string; id: string; }): boolean {
        return spec.type === this.target.type && spec.id === this.target.id;
    }
}



export class ContainerConnection {

    packetQueue       : any[];
    clientConnections : Connection[];
    connection; // ws.Connection

    constructor
    ( public delegate : WebSocketProxy
    , public host     : string
    , public port     : number
    ) {
        this.message           = this.message.bind(this);
        this.close             = this.close.bind(this);
        this.error             = this.error.bind(this);
        this.connect           = this.connect.bind(this);
        this.connectFailed     = this.connectFailed.bind(this);

        this.packetQueue       = [];
        this.clientConnections = [];


        var client = new WebSocketClient;
        client.connect("ws://" + host + ":" + port + "/");

        client.on('connectFailed', this.connectFailed);
        client.on('connect',       this.connect);

    }


    disconnectClients(reason: string): void {
        this.clientConnections.forEach(function(connection) {
            connection.disconnectWith(reason);
        });

        this.clientConnections = [];
        this.delegate.proxyShutdown();
    }

    connectFailed(error): void {
        this.disconnectClients(error.toString());
    }

    connect(connection): void {
        this.connection = connection;

        connection.on('error',   this.error);
        connection.on('close',   this.close);
        connection.on('message', this.message);

        this.packetQueue.forEach(function(x) {
            connection.send(x);
        });

        delete this.packetQueue;
    }

    error(error): void {
        this.disconnectClients(error.toString());
    }

    close(): void {
        this.disconnectClients("Container closed connection");
    }

    message(message): void {
        var json = JSON.parse(message.utf8Data);
        this.delegate.replyPacket(json.target, json.accountID, json.msg);
    }

    registerConnection(connection: Connection): void {
        this.clientConnections.push(connection);
    }

    sendPacket(data): void {
        var x = JSON.stringify(data);
        if (this.packetQueue) {
            this.packetQueue.push(x);
        } else {
            this.connection.send(x);
        }
    }

    clearConnection(connection: Connection): void {
        this.clientConnections = this.clientConnections.filter(function(x) {
            return x !== connection;
        });
    }
}
