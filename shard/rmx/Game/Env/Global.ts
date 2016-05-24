
import { controllingPlayer, IEntity, playerParty, FormulaModifier } from '../../Pure/Game';
import { State } from '../../Game/State';
import { EntityId, AccountId } from '../../Pure/Ids';
import { scheduleFuture, randomBetween, randomIntBetween, random } from '../../Game';
import { WorldObject } from '../../Game/Entities/WorldObject';
import { lookupEntity } from '../../Pure/Game';
import { checkParams } from '../../Game/Env';
import * as Env from '../../Game/Env';
import * as G from '../../Game/State';



export function mk(state: State) {
    var env: any = {};



    env.controllingPlayer = function(entity: WorldObject) {
        checkParams('global::controllingPlayer', arguments, WorldObject);
        return controllingPlayer(state, entity);
    };

    env.playerParty = function(accountId: AccountId) {
        checkParams('global::playerParty', arguments, String);
        return playerParty(state, accountId);
    };

    env.lookupEntity = function(entityId: EntityId): IEntity {
        checkParams('global::lookupEntity', arguments, String);
        return lookupEntity<IEntity>(state, entityId);
    };

    env.entityDisplayName = function(entity: WorldObject) {
        checkParams('global::entityDisplayName', arguments, WorldObject);
        return entity.name;
    };

    env.Modifier = (a, b, c, d) => {
        return new FormulaModifier(a, b, c, d);
    };



    // Randomness
    // ---------------------------------------------------------------------

    Env.defineAccessor(env, 'random', function() {
        return random(state);
    });

    env.randomBetween = function(min, max) {
        checkParams('global::randomBetween', arguments, Number, Number);
        return randomBetween(state, min, max);
    };

    env.randomIntBetween = function(min, max) {
        checkParams('global::randomIntBetween', arguments, Number, Number);
        return randomIntBetween(state, min, max);
    };



    // Async
    // -------------------------------------------------------------------
    //
    // FIXME: Should only make this available to the Glue script and maybe
    // behaviors, but nobody else.

    env.scheduleFuture = function(delay: number, tag: string, action: Function) {
        checkParams('global::scheduleFuture', arguments, Number, String, Function);
        scheduleFuture(state, delay, tag, () => {
            action();
        });
    };



    // Time
    // ---------------------------------------------------------------------

    Env.defineAccessor(env, 'currentTime', function() {
        return state.currentTime;
    });


    // The amount of time spent in the 'Running' stage.
    Env.defineAccessor(env, 'gameDuration', function() {
        return G.duration(state);
    });



    return env;
}
