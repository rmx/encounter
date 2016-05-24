/// <reference path="../Storage/Game.ts" />
/// <reference path="../Pure/Game.ts" />
/// <reference path="../data.ts" />

module rmx.Core {

    // playerClassName
    // -----------------------------------------------------------------------
    //
    // If the player has a character class selected, the computation will
    // resolve to the class name. Otherwise it remains pending.

    export function
    playerClassName(player: rmx.Pure.Player): Computation<string> {
        if (player && player.roleId) {
            return rmx.data.resolveReferenceString<string>(player.roleId + '.content.name');
        } else {
            return Computation.pending;
        }
    }



    // winningParty
    // -----------------------------------------------------------------------
    //
    // Note: not all games have a winning party!

    export function
    winningParty(game: rmx.Storage.Game): rmx.Storage.GameParty {
        var ret = null;

        game.parties.forEach(party => {
            if (party.objectives.every(x => { return x.isCompleted; })) {
                if (ret) {
                    if (party.score > ret.score) {
                        ret = party;
                    }
                } else {
                    ret = party;
                }
            }
        });

        return ret;
    }


    // resourceContent
    // -----------------------------------------------------------------------

    export function
    resourceContent<T>(resourceId: string): Computation<T> {
        return rmx.data.parseReferenceString(resourceId)
            .bind<T>(resourceContentReference);
    }


    // resourceContentReference
    // -----------------------------------------------------------------------

    export function
    resourceContentReference<T>(ref: rmx.Storage.Reference): Computation<T> {
        return rmx.data.resolveReference<rmx.Storage.Resource<T>>(ref).fmap(x => {
            return x.content;
        });
    }


    export function
    humanReadableAmount(x: number): string {
        var i = -1
          , units = ['k', 'M', 'G'];

        if (x >= 1000) {
            do {
                x = x / 1000;
                i++;
            } while (x > 1000);

            if (units[i] !== undefined) {
                return Math.max(x, 0.1).toFixed(1) + units[i];
            } else {
                return 'a lot';
            }
        } else {
            return '' + x;
        }
    };


    export function
    isTutorialEncounter(gameId: string): boolean {
        return rmx.data.findById<rmx.Storage.Game>(gameId).fmap(gameE => {
            return gameE.content.encounter.objectId === rmx.config.tutorialEncounterId;
        }).get(false);
    }
}
