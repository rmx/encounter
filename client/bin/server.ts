/// <reference path="../config/assets.ts" />

var connect     = require('connect')
  , morgan      = require('morgan')
  , serveStatic = require('serve-static');

function mkToken(path, result) {
    if (process.env.TYPEKIT && result.externalUrl) {
        return result.externalUrl;
    } else {
        return '/' + path;
    }
}

function emitAsset(id, ce, fn) {
    fn();
}

function findCacheEntry(id) {
    if (cache[id]) {
        return cache[id];
    } else {
        for (var k in cache) {
            if (cache[k].uri === id) {
                return cache[k];
            }
        }
    }
}
function assetMiddleware(req, res, next) {
    var id = req.url.substring(1, req.url.length);
    var options =
        { cache     : cache
        , mkToken   : mkToken
        , emitAsset : emitAsset
        };

    var ce = findCacheEntry(req.url);
    if (ce) {
        res.setHeader('Content-Type', ce.result.contentType);
        res.end(ce.res.content);
    } else {
        rmx.Assets.compileAsset(id, options, function(err, ce) {
            if (err) {
                console.log(err);
                next();
            } else {
                res.setHeader('Content-Type', ce.result.contentType);
                res.end(ce.result.content);
            }
        });
    }
}

var cache : any = {};

var app = connect()
    .use(morgan('tiny'))
    .use(serveStatic('tmp'))
    .use(serveStatic('app'))
    .use(assetMiddleware)
    .use(function(req, res, next) {
        req.url = '/index.html';
        next();
    })
    .use(assetMiddleware)
    .listen(3001);
