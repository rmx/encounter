/// <reference path="../config/assets.ts" />

var fs     = require('fs');
var path   = require('path');
var mkdirp = require('mkdirp');

var dist = __dirname + '/../dist/';
if (!fs.existsSync(dist)) {
    fs.mkdirSync(dist);
}

function mkTokenC(path, result) {
    if (result.externalUrl) {
        return result.externalUrl;
    } else {
        return path;
    }
}

function emitAssetC(id, ce, fn) {
    console.log(id, '->', ce.token);

    var dst = dist + ce.token;

    if (id === 'index.html') {
        var version = process.argv[2];
        ce.result.content = ce.result.content.replace(/%VERSION%/, version);
    }

    mkdirp(path.dirname(dst), function() {
        fs.writeFile(dst, ce.result.content, function() {
            fn();
        });
    });
}

var options =
    { cache     : <any> {}
    , mkToken   : rmx.Assets.fingerprintedToken
    , emitAsset : emitAssetC
    };

rmx.Assets.compileAsset('index.html', options, function(err, ce) {
    if (err) {
        throw err;
    } else {
        fs.renameSync(dist + '/' + ce.token, dist + '/index.html');
    }
});
