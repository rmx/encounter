/// <reference path="../test/node.d.ts" />

module rmx.Assets {

    export interface AssetId extends String {}



    var fs     = require('fs');
    var path   = require('path');
    var async  = require('async');
    var crypto = require('crypto');
    var mime   = require('mime');
    var spawn  = require('child_process').spawn;
    var fm     = require('front-matter');
    var md     = require('markdown').markdown;



    function binaryFile(file: string, contentType: string): Builder {
        return function(options: BuilderOptions, fn: BuilderCallback) {
            fs.readFile(__dirname + "/../" + file, function(err, content) {
                fn(err, { contentType: contentType, content: content })
            })
        }
    }

    function helpArticle(file: string): Builder {
        return function(options: BuilderOptions, fn: BuilderCallback) {
            compile(file, 'application/json')(options, function(err, res) {
                if (err) {
                    fn(err);
                } else {
                    var data = fm(res.content)
                      , tree = md.parse(data.body);

                    var refs = tree[1].references;
                    if (!refs) {
                        refs = {};
                        tree.splice(1, 0, { references: refs });
                    }

                    (function find_link_refs(jsonml) {
                        if (jsonml[0] === "link_ref") {
                            var ref = jsonml[1].ref;

                            if (ref.match(/^article:/)) {
                                refs[ref] = {
                                    href: "/help/" + ref.replace(/^article:/, "")
                                };
                            }
                        } else if (Array.isArray(jsonml)) {
                            jsonml.forEach(find_link_refs);
                        }
                    })(tree);

                    var html = md.renderJsonML(md.toHTMLTree(tree));

                    var content = JSON.stringify(
                        { attributes : data.attributes
                        , body       : html
                        }
                    );

                    fn(err, { contentType: 'application/json', content: content });
                }
            });
        }
    }

    function compile(file: string, contentType: string): Builder {
        var spliceRegExp = /<\* ([a-zA-Z0-9/._-]*) \*>/g;

        function collectSpliceCallbacks(options, content) {
            var splices = [];

            content.replace(spliceRegExp, function(_, id) {
                splices.push(function(fn) {
                    compileAsset(id, options, function(err, ce) {
                        fn(err, ce);
                    });
                });
            });

            return splices;
        }

        return function(options: BuilderOptions, fn: BuilderCallback) {
            fs.readFile(__dirname + "/../" + file, 'utf8', function(err, content) {
                if (err) {
                    fn(err);

                } else {
                    var splices = collectSpliceCallbacks(options, content);
                    async.parallel(splices, function(err) {
                        if (err) {
                            fn(err);
                        } else {
                            var newContent = content.replace(spliceRegExp, function(_, id) {
                                return options.cache[id].token;
                            });

                            fn(err, { content: newContent, contentType: contentType })
                        }
                    });
                }
            })
        }
    }


    export function
    compileAsset(id: AssetId, options: BuilderOptions, fn: CompileCallback) {
        var ce = options.cache[<string>id];
        if (ce) {
            return fn(null, ce);

        } else {
            var builder = assetDefinitions[<string>id];
            if (builder) {
                builder(options, function(err, result) {
                    if (err) {
                        fn(err);
                    } else {
                        var token = options.mkToken(id, result)
                          , ce    = { token: token, result: result };

                        options.cache[<string>id] = ce;
                        options.emitAsset(id, ce, function() {
                            fn(null, ce);
                        });
                    }
                });
            } else {
                fn(new Error('Asset ' + id + ' not found.'));
            }
        }
    }

    function hash(content: NodeBuffer): NodeBuffer {
        var h = crypto.createHash('sha');
        h.update(content);
        return h.digest();
    }

    function fingerprint(content: NodeBuffer): string {
        var bytes = Array.prototype.slice.call(hash(content), 0)
          , chars = "abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789"
          , len   = chars.length;

        return bytes
            .map(function(x) { return chars[x % len]; })
            .join('');
    }

    export function
    fingerprintedToken(id: AssetId, result: Result): string {
        if (result.externalUrl) {
            return result.externalUrl;
        } else {
            var parts  = id.split('.')
            , ext    = parts.pop();

            return ['/', fingerprint(result.content), '.', ext].join('');
        }
    }

    function contentTypeFromFilename(filename) {
        return mime.lookup(filename) || 'application/octet-stream';
    }

    function jsx(dir, module): Builder {
        return function(options: BuilderOptions, fn: BuilderCallback) {
            var child = spawn('./node_modules/react-tools/bin/jsx', [dir, './tmp', module]);
            child.on('error', function () {
                console.log('ERROR: Could not spawn jsx, make sure react-tools is installed');
            });
            child.on('close', function(code) {
                compile('tmp/' + module + '.js', '')(options, function(err, result) {
                    if (err) {
                        fn(err);
                    } else {
                        fn(err, { content: result.content, contentType: 'application/javascript' });
                    }
                });
            });
        }
    }

    function typescript(file): Builder {
        return function(options: BuilderOptions, fn: BuilderCallback) {
            var tmp = 'file' + Math.random();
            var child = spawn('./node_modules/typescript/bin/tsc', ["--target","ES6","--out", "./tmp/" + tmp, "./config/production.ts", file]);
            child.on('error', function () {
                console.log('ERROR: Could not spawn tsc, make sure typescript is installed');
            });
            child.on('close', function(code) {
                compile('tmp/' + tmp, '')(options, function(err, result) {
                    if (err) {
                        fn(err);
                    } else {
                        fn(err, { content: result.content, contentType: 'application/javascript' });
                    }
                });
            });
        }
    }

    function typekit(): Builder {
        return function(options: BuilderOptions, fn: BuilderCallback) {
            fn(null, { content: '', contentType: 'application/javascript', externalUrl: '//use.typekit.net/fce5qlg.js' });
        }
    }
    export interface CompileCallback {
        (err: Error, ce?: CacheEntry): void;
    }

    export interface CacheEntry {
        token  : string;
        result : Result;
    }

    export interface BuilderOptions {
        cache     : { [assetId: string]: CacheEntry };
        mkToken   : (id: AssetId, result: Result) => string;
        emitAsset : (id: AssetId, ce: CacheEntry, fn: () => void) => void;
    }

    export interface Result {
        content     : any;
        contentType : string;
        externalUrl?: string;
    }

    export interface BuilderCallback {
        (err: Error, result?: Result): void;
    }

    export interface Builder {
        (options: BuilderOptions, fn: BuilderCallback): void;
    }

    export interface AssetDefinitions {
        [id: string]: Builder;
    }


    export var assetDefinitions : AssetDefinitions =
    { "index.html":
        compile("assets/index.html", 'text/html')

    , "rmx.css":
        compile("app/rmx.css", 'text/css')

    , "styles.css":
        compile("assets/styles.css", 'text/css')

    , "semantic/css/semantic.css":
        compile("assets/semantic/css/semantic.css", 'text/css')

    , "react-components.js":
        jsx('jsx', 'react-components')

    , "entry.js":
        typescript('app/entry.ts')

    , "typekit":
        typekit()
    };

    var walk = require('walk');
    walk.walkSync('./assets',
        { listeners:
            { file:
                function(root, fileStats, next) {
                    var file = root + '/' + fileStats.name
                      , id   = file.replace(/^.\/assets\//, '');

                    if (!assetDefinitions[id]) {
                        assetDefinitions[id] = binaryFile(file, contentTypeFromFilename(file));
                    }
                }
            }
        }
    );

    walk.walkSync('./help',
        { listeners:
            { file:
                function(root, fileStats, next) {
                    var file = root + '/' + fileStats.name
                      , id   = file.replace(/^.\/help/, 'help');

                    if (!assetDefinitions[id]) {
                        assetDefinitions[id] = helpArticle(file);
                    }
                }
            }
        }
    );
}
