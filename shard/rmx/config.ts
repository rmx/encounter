/// <reference path="../lib/node.d.ts" />

var config = {
    apiHost: undefined,
    encounterId: undefined,
    enableWebSocketProxy: false,
};

export default config;



// apiHost
// -----------------------------------------------------------------------
//
// The base url to the API. Can be overridden by setting 'API' environment
// variable.

Object.defineProperty(config, 'apiHost', {
    get: () => {
        return process.env.API || 'https://api.rmx.im';
    }
});


// encounterId
// -----------------------------------------------------------------------
//
// Set if the shard should always load a particular encounter when
// starting a game. This is mainly useful for local development.

Object.defineProperty(config, 'encounterId', {
    get: () => {
        return process.env.ENCOUNTER;
    }
});


// enableWebSocketProxy
// -----------------------------------------------------------------------
//
// True if the shard should start a local websocket proxy. Mainly useful
// for local development.

Object.defineProperty(config, 'enableWebSocketProxy', {
    get: () => {
        return !!process.env.WEBSOCKETPROXY;
    }
});
