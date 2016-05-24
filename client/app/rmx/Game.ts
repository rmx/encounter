/// <reference path="../ext/components.ts" />
/// <reference path="./Game/Client.ts" />


module rmx {

    // Allocate a new game on the server for the given encounter. The callback
    // will be given the gameId which can be used to open the game.
    export function
    createGame
    ( encounterId : string
    , purpose     : rmx.Storage.Purpose
    , fn          : Function
    ): void {
        var url = rmx.config.apiHost + '/games';

        var data =
            { encounterId : encounterId
            , purpose     : rmx.Pure.JSON.enumToJSON(rmx.Storage.Purpose, purpose)
            };

        request
            .post(url)
            .send(data)
            .withCredentials()
            .end(function(res: any) {
                if (res.status === 200) {
                    fn(null, res.body.gameId);
                } else {
                    try {
                        var json = JSON.parse(res.text);
                        fn(new Error("Failed to create game: " + json.error));
                    } catch (e) {
                        fn(new Error("Failed to create game: " + e.message));
                    }
                }
            });
    }


    // Open an existing game and return its controls, so that the client can
    // interact with it.
    export function openGame(gameId: string): rmx.Game.Client {
        rmx.data.activeGame = rmx.data.games[gameId];
        if (!rmx.data.activeGame) {
            rmx.data.activeGame = new rmx.Game.Client(gameId);
            rmx.data.games[gameId] = rmx.data.activeGame;
        }

        return rmx.data.activeGame;
    }
}


module rmx.data {

    // The game which is currently running in the foreground.
    export var activeGame : rmx.Game.Client = null;


    export var games : { [gameId: string]: rmx.Game.Client } = {};

}
