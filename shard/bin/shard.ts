import * as os from 'os';
import * as dns from 'dns';
import * as url from 'url';

import request from 'request';
import influx from 'influx';

import config from '../rmx/config';
import * as Shard from '../rmx/Shard/Server';
import { WebSocketProxy } from '../rmx/WebSocketProxy';




// Parse configuration given on the commandline (key=value pairs).
var msg = <any> process.argv.slice(2).reduce(function(x, opt) {
    var kv = opt.split('=');
    x[kv[0]] = kv[1] || true;
    return x;
}, {});


msg.externalPort = parseInt(msg.externalPort || process.env.PORT || 3010, 10);
msg.internalPort = parseInt(msg.internalPort || msg.externalPort, 10);


// Adjust the process title and start the server.
process.title += ' shard/X' + '@' + msg.externalPort;
console.log("Shard port allocation:", msg.externalPort, "->", msg.internalPort);

if (config.enableWebSocketProxy) {
    new WebSocketProxy().httpServer().listen(3020);
    console.log('WebSocketProxy running on port 3020');
}

var version = null;
try {
    version = require('../version');
    console.log('version:', version);
} catch (e) {
    console.log('Could not load version: ' + e);
}

function publicAddress(): Promise<string> {
    return new Promise((resolve, reject) => {
        request({
            url: 'http://metadata.google.internal/computeMetadata/v1/instance/network-interfaces/0/ip',
            headers: {'Metadata-Flavor': 'Google' },
        }, function(err, res) {
            if (err) {
                console.log('instance metadata service not available: ', err.toString());
                dns.lookup(os.hostname(), function(err, address) {
                    if (err) {
                        console.log('DNS lookup failed: ', err);
                        reject(err);
                    } else {
                        resolve(address);
                    }
                });
            } else {
                resolve((<any>res).body);
            }
        });
    });
}

function registerShard(fn) {
    if (version === null) {
        return fn(null);
    }

    publicAddress().then(addr => {
        var externalAddress = msg.externalAddress || addr;

        var body =
            { version : version
            , address : [externalAddress, msg.externalPort]
            };

        console.log('Reachable at ws://' + externalAddress + ':' + msg.externalPort);

        var options =
            { url  : config.apiHost + '/shards'
            , body : JSON.stringify(body)
            };

        request.post(options, function(err, response, body) {
            (function(...args){ return args; })(err, response);

            var json = JSON.parse(body);

            if (json.shardId) {
                fn(json.shardId);
            } else {
                console.log("registerShard: failed");
            }
        });
    }).catch(err => {
        console.log('Could not get public address:', err);
        fn(null);
    });
}

registerShard(shardId => {
    var server = new Shard.Server(shardId, msg.internalPort);

    console.log('Registered as shard ' + shardId);

    reportHealth();
    setInterval(reportHealth, 5000);

    function reportHealth() {
        var load = os.loadavg();

        var options =
            { url  : config.apiHost + '/shards/' + shardId + '/health'
            , body : JSON.stringify({ load : load[0] })
            };

        request.put(options, function(err, response, body) {
            (function(...args){ return args; })(err, response, body);
        });
    }


    var influxUrl = process.env.INFLUX;
    if (influxUrl) {
        var u = url.parse(influxUrl);
        var auth = u.auth.split(':');

        var influxClient = influx(
            { hosts    : [ { host: u.hostname, port: parseInt(u.port, 10) } ]
            , database : u.pathname.slice(1, u.pathname.length)
            , username : auth[0]
            , password : auth[1]
            }
        );


        // shard.load
        setInterval(() => {
            var load = os.loadavg();
            influxClient.writePoint('shard.load', { value: load[0] }, { shard: shardId }, () =>{});
        }, 1000);


        // shard.latency
        setInterval(() => {
            var samples = server.webSocketServer.latencySamples
              , latency = samples[samples.length - 1];

            if (latency !== undefined) {
                influxClient.writePoint('shard.latency', { value: latency }, { shard: shardId }, () => {});
            }
        }, 1000);
    }
});
