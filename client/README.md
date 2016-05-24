# encounter-client

Shiny new client for encounter the game..


## Quick start

In order to run the encounter-client install the following dependencies:

* [nodejs](http://nodejs.org/) v0.10.x
* [rethinkdb](http://www.rethinkdb.com) v1.11.2

Install all dependencies and bootstrap all the tools required to develop
the client:

    ./script/bootstrap


The server listens on port 3001 and serves the client. The first time you
load the client it will take a while, because the server has to compile
the typescript code, which is slow. All future requests are quick because
the server caches the output.

Make sure to enable experimental js features in Chrome (about:flags).

    node bin/server
    xdg-open http://localhost:3001


Start the gulp watch task to continuously recompile the source files:

    ./node_modules/.bin/gulp watch

## Local client, live database

In order to run the encounter-client against the live database, compile
the code using

    ./node_modules/.bin/gulp semilocal

## Editing the client

Important files:

 - `app/routes.ts`
 - `app/views.ts`
 - `jsx/react-components.js`
 - `app/rmx/types/*`
 - `app/rmx/objects/encounter.ts`

## License

The code is under BSD license.
