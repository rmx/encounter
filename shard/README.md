## Getting started

The server is written in [node.js](http://nodejs.org/). You should install
version 0.10.x or later.

After you checkout the repository, you are just two commands away from running
the shard:

    ./script/bootstrap
    WEBSOCKETPROXY=YesPlease node bin/shard

On the production system the websocket proxy is a separate process. But the
shard can provide its own for local use. You probably want to enable it.

If you want to work on the game engine, you should run open a separate
terminal and run the typescript compiler in watch mode to continuously
recompile the dev server.

    ./node_modules/.bin/tsc --watch --target ES6 \
        --out bin/shard bin/shard.ts

Note that you need to restart the `shard` after each change.

By defaut the shard will connect to the production api (https://api.rmx.im).
If you have a local api server, you can change that by setting the `API`
environment variable:

    WEBSOCKETPROXY=YesPlease API=http://localhost:8000 node bin/shard

You can also specify a particular encounter which you want to start whenever
a client connets to the shard:

    WEBSOCKETPROXY=YesPlease ENCOUNTER=TOuhroQZWm node bin/shard

The shard catches all exceptions and aborts the game. For debugging it's often
easier to print the exception (and its stacktrace) and abort the game:

    EXCEPTIONS=throw node bin/shard


## The Shell

The shell is just like the nodejs shell, but gives you access to the whole
rmx module. It is useful if you want to test functions interactively.

Compile the shell like this:

    ./node_modules/.bin/tsc --watch --target ES6 \
        --out bin/shell bin/shell.ts

And run it with:

    node bin/shell

The prompt will show `shard>`. Since we put all our classes and functions into
a top-level module, you can access all of that stuff through it, for example:

    shard> rmx.Pure.dasherize('FooBarBaz')
    '-foo-bar-baz'
    shard>
