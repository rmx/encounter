import * as Storage from '../Storage';
import { State } from '../Game/State';
import { SpellTarget, Reaction, entityParty, playerControlledEntityId, lookupEntity }
    from '../Pure/Game';
import { WorldObject, distanceToSpellTarget, reactionTowards } from '../Game/Entities/WorldObject';

export interface TargetResolver {
    ( state     : State
    , caster    : WorldObject
    , spellInfo : Storage.Spell
    , a         : any
    ): any;
}


export function
lookupTargetResolverImpl(fn: Storage.TargetResolverFunction): TargetResolver {
    if (fn.content instanceof Storage.TRFSelf) {
        return TargetResolvers.self;

    } else if (fn.content instanceof Storage.TRFParty) {
        return TargetResolvers.party;

    } else if (fn.content instanceof Storage.TRFPartyInArea) {
        return TargetResolvers.partyInArea;

    } else if (fn.content instanceof Storage.TRFEnemiesInArea) {
        return TargetResolvers.enemiesInArea;
    }
}



export module TargetResolvers {

    export function
    self
    ( state     : State
    , caster    : WorldObject
    , spellInfo : Storage.Spell
    , a         : any
    ): any {
        // FIXME: Hack to silence unused variable warning.
        (() => { return state; })();
        (() => { return spellInfo; })();
        (() => { return a; })();

        return new SpellTarget(caster.id, undefined);
    }



    export function
    party
    ( state     : State
    , caster    : WorldObject
    , spellInfo : Storage.Spell
    , a         : any
    ): any {
        // FIXME: Hack to silence unused variable warning.
        (() => { return spellInfo; })();
        (() => { return a; })();

        var party = entityParty(state, caster);
        if (party) {
            return party.players.map((p) => {
                var entityId = playerControlledEntityId(state, p.accountId);
                if (entityId) {
                    return new SpellTarget(entityId, undefined);
                }
            }).filter(x => {
                return !!x;
            });

        } else {
            return [];
        }
    }



    export function
    partyInArea
    ( state     : State
    , caster    : WorldObject
    , spellInfo : Storage.Spell
    , a         : any
    ): any {
        return party(state, caster, spellInfo, a).filter(spellTarget => {
            var entity = lookupEntity<WorldObject>(state, spellTarget.entityId);
            if (entity) {
                var reaction    = reactionTowards(entity, caster)
                  , distance    = distanceToSpellTarget(state, entity, a)
                  , maxDistance = spellInfo.radius.base;

                return reaction == Reaction.Friendly && distance <= maxDistance;

            } else {
                return false;
            }
        });
    }



    export function
    enemiesInArea
    ( state     : State
    , caster    : WorldObject
    , spellInfo : Storage.Spell
    , a         : any
    ): any {

        // XXX: TypeScript doesn't seem to support for..of iteration.
        var entities = [];
        state.entities.forEach(entity => {
            entities.push(entity);
        });

        return entities.filter(x => {
            return x instanceof WorldObject;
        }).filter((x: WorldObject) => {
            var reaction    = reactionTowards(x, caster)
              , distance    = distanceToSpellTarget(state, x, a)
              , maxDistance = spellInfo.radius.base;

            return reaction == Reaction.Hostile && distance <= maxDistance;
        }).map(x => {
            return new SpellTarget(x.id, undefined);
        });
    }
}
