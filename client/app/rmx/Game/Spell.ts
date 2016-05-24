/// <reference path="../../ext/computation.ts" />
/// <reference path="../../ext/gl-matrix.ts" />

/// <reference path="../data.ts" />

/// <reference path="./Client.ts" />
/// <reference path="./Entities/WorldObject.ts" />


module rmx.Game {

    export function
    isSpellReady
    ( client   : Client
    , entity   : WorldObject
    , spellRef : rmx.Storage.Reference
    ): Computation<boolean> {
        if (spellCooldownLeft(client, entity, spellRef.toString()) > 0) {
            return Computation.pure(false);
        } else {
            return rmx.Core.resourceContentReference<rmx.Storage.Spell>(spellRef).fmap(spell => {
                if (spell.targetType === 'self') {
                    return true;

                } else if (spell.targetType === 'world-object') {
                    var target = rmx.Pure.lookupEntity<WorldObject>(client.state, entity.targetId);

                    if (target) {
                        var distance = vec3.dist(entity.position, target.position);
                        return distance >= spell.range.min && distance <= spell.range.max;
                    } else {
                        return true;
                    }
                } else {
                    return true;
                }
            });
        }
    }


    export function
    spellCooldownLeft
    ( client  : Client
    , entity  : WorldObject
    , spellId : string
    ): number {
        var timer = entity.spellCooldowns.get(spellId);
        if (timer && timer.isActive(client.serverTime)) {
            var left = timer.start + timer.duration - client.serverTime;
            return (client.localTime + left) - rmx.now();
        } else {
            return 0;
        }
    }
}
