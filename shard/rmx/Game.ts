/// <reference path="../lib/node.d.ts" />

import { Vec3, vec3 } from 'gl-matrix';

import { EntityId, AccountId } from './Pure/Ids';
import { Soul, lookupEntity, registerEntity, Combinator,
    Bounds, Formula, Stage, Party, Objective, Player,
    objectivesCompleted, IEntity } from './Pure/Game';
import { Terrain, TerrainPosition, getWorldCoordinates, terrainPositionAt } from './Pure/Terrain';
import { State, ScoringFunction, Action, Future } from './Game/State';
import { SMSG_SPELL_COOLDOWNS } from './Game/Messages';
import * as Storage from './Storage';
import { WorldObject, createWorldObject, isAlive, modifyPower,
    processEvent_WorldObject } from './Game/Entities/WorldObject';
import * as Message from './Pure/Message';
import { OldGlobal } from './Game/Global';
import * as PT from './Pure/Task';
import { SMSG_INITIAL_SPELLS, SMSG_CONTROL } from './Game/Messages';
import * as Task from './Game/Tasks';
import { PathFinder } from './Pure/PathFinder';
import * as AS from './Avers/Storage';
import * as Protocol from './Pure/Protocol';
import * as Decoder from './Game/Decoder';
import { ContentProvider, referenceToString } from './Game/ContentProvider';
import * as ScoringFunctionEnv from './Game/Env/ScoringFunction';
import { PowerRegenEvent } from './Pure/Game/Events';
import * as M from './Game/Messages';

import { _ } from 'underscore';
var vm                  = require('vm');
var CoffeeScript        = require('coffee-script');
var assert              = require('assert');



export enum MoveFlags {
    None        = 0x00000000,

    Forward     = 0x00000001,
    Backward    = 0x00000002,

    StrafeLeft  = 0x00000004,
    StrafeRight = 0x00000008,

    TurnLeft    = 0x00000010,
    TurnRight   = 0x00000020,
}


var sum               = Combinator.sum;
var product           = Combinator.product;
var stdCombinator     = new Combinator(sum, product, sum, product);
var stdEvaluator      = (x, a, b, c, d) => { return (((x + a) * b) + c) * d; };

export var stdFormula = new Formula
    ( stdCombinator
    , stdEvaluator
    , new Bounds(0, 99999999)
    );


// Same as 'stdFormula' but the lower bound is one instead of zero.
export var maxHealthFormula = new Formula
    ( stdCombinator
    , stdEvaluator
    , new Bounds(1, 99999999)
    );



export class NoScript {}

export function arraysNotEqual(a, b) {
    var i, x, _i, _len;
    if (a.length !== b.length) {
        return true;
    }
    for (i = _i = 0, _len = a.length; _i < _len; i = ++_i) {
        x = a[i];
        if (x !== b[i]) {
            return true;
        }
    }
    return false;
}



// Scheduler
// -------------------------------------------------------------------------

export function
scheduleFuture
( state  : State

, delay  : number
  // ^ The delay (in seconds) after which to execute the action. Must be
  // positive.
  //
  // TODO: Figure out what to do if the delay is negative. Should that
  // be a fatal error? Or a warning?

, tag    : string
  // ^ The tag is used for debugging, to have at least a bit of an idea
  // where a future is coming from.

, action : Action
  // ^ The action to execute after the delay.

): void {
    if (state.futures.length >= 1000) {
        throw new Error(
            [ "The state contains too many futures ("
            , state.futures.length
            , ")"
            ].join('')
        );
    }

    var future: Future =
        { futureId  : state.futureCounter++
        , executeAt : state.currentTime + delay
        , tag       : tag
        , action    : action
        };

    state.futures.push(future);
    state.futures.sort(function(lhs, rhs) {
        var diff = lhs.executeAt - rhs.executeAt;
        if (diff === 0) {
            return lhs.futureId - rhs.futureId;
        } else {
            return diff;
        }
    });
}



// Outgoing message dispatch
// -------------------------------------------------------------------------

function
deliverMessage(state: State, msg: M.Message, set: AccountId[]): void {
    state.outgoingMessages.push({ msg: msg, set: set });
}

export function
messageToAccount(state: State, accountId: AccountId, msg: M.Message): void {
    deliverMessage(state, msg, [accountId]);
}

export function
broadcastMessage(state: State, msg: M.Message): void {
    var allAccountIds = [];
    state.parties.forEach(party => {
        party.allAccountIds.forEach(accountId => {
            allAccountIds.push(accountId);
        });
    });

    deliverMessage(state, msg, allAccountIds);
}

export function
broadcastMessageSkip(state: State, entity: WorldObject, msg: M.Message): void {
    var set = [];
    state.parties.forEach(party => {
        party.players.forEach(player => {
            if (entity.id !== player.entityId) {
                set.push(player.accountId);
            }
        });
    });

    deliverMessage(state, msg, set);
}

export function
directMessage(state: State, entity: WorldObject, msg: M.Message): void {
    var set = [];
    state.parties.forEach(party => {
        party.players.forEach(player => {
            if (entity.id === player.entityId) {
                set.push(player.accountId);
            }
        });
    });

    deliverMessage(state, msg, set);
}



// emitEvent
// -----------------------------------------------------------------------
//
// Events are emitted when the game state changes in a certain way. They
// are consumed by spells, auras, behaviors and scripts, which can then
// react to these changes.

export function
emitEvent(state: State, event: any): void {

    // Send the event to all WorldObjects (behaviors).
    state.entities.forEach(function(x) {
        if (x instanceof WorldObject) {
            processEvent_WorldObject(x, event);
        }
    });


    // Allow objective tasks to update their internal state.
    state.parties.forEach(party => {
        party.objectives.forEach(objective => {
            objective.tasks.forEach(task => {
                Task.processEvent(state, event, task, party);
            });
        });
    });


    // Update score. The expression has side effects which update the
    // individual party scores. We ignore the return value here.

    state.scoringFunction(event);
}

// forEachEntity
// -----------------------------------------------------------------------
//
// Iterate over all entities of the given type.

export function
forEachEntity<T extends IEntity>
( state : State
, ctor  : any // new (...args) => T -- FIXME: tslint has issues
, fn    : (entity: T) => void
): void {
    state.entities.forEach(x => {
        if (x instanceof ctor) {
            fn(<T>x);
        }
    });
}


// lookupResource
// -----------------------------------------------------------------------

export function
lookupResource<T>(state: State, id: string, type: string): T {
    var res = state.contentProvider.resourceMap[id];
    if (res) {
        if (res.type === type) {
            return res.value;
        } else {
            throw new Error("Wrong resource type," + id + " has type " + res.type + ", expected: " + type);
        }
    } else {
        throw new Error("Resource not found: " + id + ', type: ' + type);
    }
}


// destroyEntity
// -----------------------------------------------------------------------

export function
destroyEntity(state: State, id: EntityId): void {
    var entity = lookupEntity(state, id);
    if (entity) {
        state.entities.delete(id);
        broadcastMessage(state, new Message.SMSG_DESTROY_ENTITY(entity.id));
    }
}



// regenPower
// -----------------------------------------------------------------------
//
// Periodic function which regenerates power of all entities. It runs in
// a fixed interval, and is not cancelable.

export function
regenPower(state: State) {
    forEachEntity<WorldObject>(state, WorldObject, worldObject => {
        if (isAlive(worldObject)) {
            modifyPower(state, worldObject, 'energy', +5);
        }
    });

    scheduleFuture(state, 5, 'regenPower', regenPower);
    emitEvent(state, new PowerRegenEvent());
}


// debug
// -----------------------------------------------------------------------

export function
debug(state: State, text: string): void {
    // FIXME: Only send the message if the players are actually interested
    // (it is a test game, or admins explicitly request it).
    broadcastMessage(state, new Message.SMSG_DEBUG(text));
}


// loadScript
// -----------------------------------------------------------------------

export function
loadScript(name: string, source: string, env: Object) {
    var script = "Error.prepareStackTrace = (_, x) -> x" + '\n' + source
      , src    = CoffeeScript.compile(script, { bare: true })
      , ctx    = vm.createContext(env);

    vm.runInContext(src, ctx, name);

    return ctx.module.exports;
}


// uniqueId
// -----------------------------------------------------------------------

export function
uniqueId(state: State): EntityId {
    return (state.idCounter++).toString();
}


// setTeamScore
// -----------------------------------------------------------------------
//
// XXX: MP
// XXX: The score should never fall below zero. Clamp between zero
// and some max (TBD).

export function
setTeamScore(state: State, score: number): void {
    state.parties[0].score = score;
    broadcastMessage(state, new Message.SMSG_SCORE(state.parties[0].id, score));
}


// addTeamScore
// -----------------------------------------------------------------------

export function
addTeamScore(state: State, deltaScore: number): void {
    setTeamScore(state, state.parties[0].score + deltaScore);
}


// createTerrain
// -----------------------------------------------------------------------

export function
createTerrain
( state      : State
, resourceId : string
, entityId   : EntityId
) {
    var res = lookupResource<Storage.Terrain>(state, resourceId, 'terrain');

    var instances = res.tiles.map(x => {
        return { id       : x.id
               , tileId   : x.tile.toString()
               , position : x.position
               , rotation : x.rotation
               };
    });

    var terrain = new Terrain
        ( entityId
        , resourceId
        , state.contentProvider.tileMap
        , instances
        );

    registerEntity(state, terrain);

    broadcastMessage(state, new Message.SMSG_CREATE_TERRAIN(terrain));
}


// createGame
// -----------------------------------------------------------------------
//
// Create a new game in initial state.

export function
createGame(contentProvider: ContentProvider): State {
    var state = new State(contentProvider);


    // Run all state modifications which can throw an exception inside
    // the 'modifyState' callback.
    modifyState(state, () => {

        // Compile the scoring function.
        state.scoringFunction = compileScoringFunction(state);

        // Instantiate parties and their objectives.
        contentProvider.parties.forEach(party => {
            var partyId = (<any>party).id
              , p       = new Party(partyId);

            state.parties.push(p);

            party.objectives.forEach(ref => {
                var objectiveId = referenceToString(ref)
                  , objective   = lookupResource<Storage.Objective>(state, objectiveId, 'objective');

                var tasks = objective.tasks.map(PT.toTask);

                p.objectives.push(new Objective(objectiveId, tasks));
            });
        });


        // Instantiate terrains
        contentProvider.terrainInstances.forEach(x => {
            var entityId = contentProvider.encounterId + ':terrainInstances.' + x.id;
            createTerrain(state, x.terrain.toString(), entityId);
        });


        // Create the glue at the very end. It may depend on any fields
        // we initialized just above.
        state.glue = createGlue(state, contentProvider.glue);


        // Start global, persistent futures.
        regenPower(state);
    });


    return state;
}



// compileScoringFunction
// -----------------------------------------------------------------------
//
// FIXME: Currently scoring function is an expression. Once we add
// more types we'll have to update this.

function
compileScoringFunction(state: State): ScoringFunction {
    var scoringFunction = state.contentProvider.scoringFunction;

    assert
        ( scoringFunction.content instanceof Storage.Expression
        , 'Scoring function content is not an expression'
        );

    return compileExpression
        ( scoringFunction.content
        , ScoringFunctionEnv.mk(state)
        , ['event']
        );
}



// createGlue
// -----------------------------------------------------------------------
//
// Instanciate the glue script.

export function
createGlue(state: State, source: string): any {
    var env    = new OldGlobal(state, state, state)
      , Script = loadScript('glue', source, env);

    return new (Script || NoScript);
}


// processMessage
// -----------------------------------------------------------------------
//
// This is the entry point used to update the game state.

export function
processMessage
( state     : State
, time      : number
, accountId : AccountId
, json      : any
): void {
    modifyState(state, () => {
        if (time > 20 * 60) {
            throw new Error("Exceeded game duration limit");
        }

        state.currentTime = time;
        dispatchMessage(state, accountId, json, Decoder);

        // Process futures
        var futureCounter = 0;
        while (state.futures.length > 0 && state.futures[0].executeAt <= time) {
            if (futureCounter++ >= 20) {
                throw new Error("Number of events in current tick exceeds limit");
            }

            var future = state.futures.shift();
            future.action(state);
        }


        // Update objective completion. But only while the game is running.
        // During the setup stage and once the game is finished, the
        // objectives can not be updated anymore.

        if (state.stage === Stage.Running) {
            state.parties.forEach(party => {
                party.objectives.forEach(objective => {
                    updateObjectiveCompletion(state, party, objective);
                });
            });
        }


        // Check if a party has finished all its objectives and if yes then
        // finish the game. This check is done at the end of this time step
        // so it is possible for two or more parties to finish the game at the
        // same time. In that case they all win the game.

        if (state.stage !== Stage.Finished) {
            state.parties.forEach(party => {
                if (objectivesCompleted(party)) {
                    transitionToFinished(state);
                }
            });
        }


        // The game automatically ends if all players are dead.

        if (state.stage === Stage.Running) {
            var everybodyDead = state.parties.every(party => {
                if (party.players.length > 0) {
                    return party.players.every(player => {
                        // This lookup may fail because player entities are
                        // created asynchronously. The player is considered
                        // alive when he/she doesn't have an entity yet.
                        var entity = lookupEntity<WorldObject>(state, player.entityId);
                        if (entity) {
                            return entity.soul === Soul.Fading;
                        } else {
                            return false;
                        }
                    });

                } else {
                    // Empty parties are skipped. This can happen for example
                    // when the encounter defines multiple parties but one
                    // or more haven't been picked by any player.
                    return true;
                }
            });

            if (everybodyDead) {
                transitionToFinished(state);
            }
        }
    });
}


function
dispatchMessage
( state         : State
, accountId     : AccountId
, json          : any
, dispatchTable : any
): void {
    var opName = Protocol.opName(json.opcode)
      , fn     = dispatchTable[opName];

    if (fn) {
        fn(state, accountId, json);
    } else {
        throw new Error('No handler for op ' + opName);
    }
}



// nextEventAt
// -----------------------------------------------------------------------
//
// Return the time at which the next future in the game needs to be
// processed. This is used by the container to schedule a timeout which
// will deliver a CLOCK message to the game at the appropriate time.

export function
nextEventAt(state: State): number {
    var future = state.futures[0];
    if (future) {
        return future.executeAt;
    }
}


// lookupAccount
// -----------------------------------------------------------------------
//
// Lookup a 'Storage.Account'. This function is asynchronous,
// because the account has to be fetched through the public rmx http API.

function
lookupAccount
( accountId : AccountId
, cb        : (err, account: Storage.Account) => void
): void {
    AS.findById(<string>accountId, function(err, account) {
        cb(err, account);
    });
}


// createPlayerEntity
// -----------------------------------------------------------------------
//
// Create an entity for the given player, based on his character choice.
// This is used once the the Setup stage is concluded and the game starts.
//
// This is the only place that where we have asynchronous code.

export function
createPlayerEntity
( state  : State
, party  : Party
, player : Player
): void {
    lookupAccount(player.accountId, function(err, account) {
        modifyState(state, () => {
            if (err) {
                throw err;
            }

            var template =
                { name     : account.login
                , position : spawnPosition(state, party)
                , faction  : party.id
                };

            var roleId   = player.roleId
              , cls      = lookupResource<Storage.Class>(state, roleId, 'class')
              , creature = cls.creature
              , health   = intFromRange(state, creature.health)
              , wotempl  = _.extend({}, creature, { health: health, behavior: null }, template)
              , entity   = createWorldObject(state, wotempl);

            player.entityId = entity.id;

            messageToAccount(state, player.accountId, SMSG_INITIAL_SPELLS(entity));
            messageToAccount(state, player.accountId, SMSG_SPELL_COOLDOWNS(entity));
            broadcastMessage(state, SMSG_CONTROL(player));
        });
    });
}



// intFromRange
// -----------------------------------------------------------------------

export function
intFromRange(state: State, range: Storage.Range): number {
    return randomIntBetween(state, range.min, range.max);
}



function
spawnPosition
( state : State
, party : Party
): TerrainPosition {
    var position = null;

    var encounter = lookupResource<Storage.Encounter>(state, state.encounterId, 'encounter');
    encounter.parties.forEach(x => {
        if (x.id === party.id) {
            var entityId      = x.spawnPoint.terrainEntityId
              , terrainEntity = lookupEntity<Terrain>(state, entityId);

            // TODO: Pick a random POI instead of the last one.
            pointsOfInterestByName(state, entityId, x.spawnPoint.pointOfInterestName).forEach(poi => {
                position = terrainPositionAt(terrainEntity, <any>poi.position);
            });
        }
    });

    if (position === null) {
        throw new Error("spawnPosition: Not defined for party " + party.id);
    }

    return position;
}



// modifyState
// -----------------------------------------------------------------------
//
// Wraps a state-modification function and handles all exceptions.

function
modifyState
( state : State
, fn    : Function
): void {
    if (state.error === null) {
        try {
            fn();
        } catch (e) {
            // This is just for local debugging (or to see the error in
            // the server console.).
            console.log('modifyState:', e);

            // Save the error in the state.
            state.error = e;

            // Immediately disconnect all clients.
            broadcastMessage(state, new Message.SMSG_DISCONNECT(e.message));
        }
    }
}



// transitionToStage
// -----------------------------------------------------------------------
//
// Transition to the given stage. If the transition is invalid, throws an
// exception. This exception should be treated as an internal error.

function
transitionToStage(state: State, stage: Stage): void {
    if (stage >= state.stage) {
        if (state.stage !== stage) {
            state.stage = stage;
            broadcastMessage(state, new Message.SMSG_CHANGE_STAGE(stage));
        }

    } else {
        var from = Stage[state.stage]
          , to   = Stage[stage];

        throw new Error('Invalid stage transition: ' + from + ' -> ' + to);
    }
}


export function
transitionToRunning(state: State): void {
    transitionToStage(state, Stage.Running);
    state.duration = state.currentTime;
}

export function
transitionToFinished(state: State): void {
    transitionToStage(state, Stage.Finished);
    state.duration = state.currentTime - state.duration;
}



// createTimer
// -----------------------------------------------------------------------

export function
createTimer
( state    : State
, delay    : number
, repeats  : number
, callback : Function
): void {
    scheduleFuture(state, delay, 'timer', action(repeats));

    function action(rep: number) {
        return function(state: State): void {
            callback();
            if (rep > 1) {
                scheduleFuture(state, delay, 'timer', action(rep - 1));
            }
        };
    }

}



// Randomness
// -------------------------------------------------------------------

export function
random(state: State): number {
    return state.prng.random();
}

export function
randomBetween(state: State, min: number, max: number): number {
    return state.prng.randomBetween(min, max);
}

export function
randomIntBetween(state: State, min: number, max: number): number {
    return state.prng.randomIntBetween(min, max);
}



// PathFinder
// -----------------------------------------------------------------------

// FIXME: Type of return value?
// FIXME: 'lookupEntity' may fail, handle that case.
function
posToNavMeshFace(state: State, pos: TerrainPosition) {
    var terrain      = lookupEntity<Terrain>(state, pos.terrainId)
      , tile         = terrain.tileInstances[pos.tileInstanceId]
      , pos_vec      = getWorldCoordinates(terrain, pos)
      , min_dist     = 1e9
      , closest_face = null
      , _ref         = tile.navMeshFaces;
      // 'TerrainTileInstance.navMeshFaces' is inaccessible.

    for (var _i = 0, _len = _ref.length; _i < _len; _i++) {
        var face     = _ref[_i]
          , centroid = face.computeCentroid()
          , dist     = vec3.dist(centroid, pos_vec);

        if (dist < min_dist) {
            closest_face = face;
            min_dist = dist;
        }
    }

    return closest_face;
}


// FIXME: 'lookupEntity' may fail, handle that case.
export function
computeWaypoints
( state   : State
, from    : TerrainPosition
, to      : TerrainPosition
, backoff : number
): Vec3[] {
    var fromFace  = posToNavMeshFace(state, from)
      , terrain   = lookupEntity<Terrain>(state, from.terrainId)
      , toFace    = posToNavMeshFace(state, to)
      , path      = new PathFinder(terrain, fromFace, toFace, from, to)
      , waypoints = path.compute();

    if (!waypoints) {
        return [getWorldCoordinates(terrain, from)];
    } else {
        while (waypoints.length >= 2) {
            var last_waypoint        = waypoints.pop()
              , second_last_waypoint = waypoints.pop()
              , length               = vec3.dist(last_waypoint, second_last_waypoint);

            if (length < backoff) {
                backoff -= length;
                waypoints.push(second_last_waypoint);
                continue;
            }

            var direction = vec3.create();
            vec3.sub(direction, last_waypoint, second_last_waypoint);
            vec3.normalize(direction, direction);
            vec3.scale(direction, direction, backoff);
            vec3.add(last_waypoint, last_waypoint, direction);
            waypoints.push(second_last_waypoint);
            waypoints.push(last_waypoint);
            break;
        }

        return waypoints;
    }
}


// updateObjectiveCompletion
// -----------------------------------------------------------------------

function
updateObjectiveCompletion
( state     : State
, party     : Party
, objective : Objective
): void {
    if (!objective.isCompleted) {
        objective.isCompleted = objective.tasks.every(task => {
            return Task.isCompleted(state, task, party);
        });

        if (objective.isCompleted) {
            var msg = new Message.SMSG_UPDATE_OBJECTIVE
                ( party.id
                , objective.id
                , objective.isCompleted
                );

            broadcastMessage(state, msg);
        }
    }
}



// pointsOfInterestByName
// -----------------------------------------------------------------------

export function
pointsOfInterestByName
( state    : State
, entityId : EntityId
, name     : string
): Storage.PointOfInterest[] {
    var entity = lookupEntity<Terrain>(state, entityId);
    if (entity) {
        var terrain = lookupResource<Storage.Terrain>(state, entity.resourceId, 'terrain');

        return terrain.pointsOfInterest.filter(poi => {
            return poi.name === name;
        });
    } else {
        return [];
    }
}



// setScore
// -----------------------------------------------------------------------

export function
setScore(state: State, party: Party, score: number): void {
    if (score < 0 || score == +Infinity) {
        throw new Error("Attempted to set the score outside of the allowed range: " + score);
    }

    if (party.score !== score) {
        party.score = score;
        broadcastMessage(state, new Message.SMSG_SCORE(party.id, score));
    }
}



// runExpression
// -----------------------------------------------------------------------
//
// Run the expression in the given environment and return the return
// value.

export function
runExpression<T>
( expression : Storage.Expression
, env        : any
): T {
    return compileExpression<T>(expression, env, [])();
}



// compileExpression
// -----------------------------------------------------------------------
//
// Compile an expression into a function which takes some arguments and
// returns a value of type T.

export function
compileExpression<T>
( expression : Storage.Expression
, env        : any
, argNames   : string[]
): (...args: any[]) => T {
    // Indent all lines by a bit to allow multi-line expressions.
    var source =
        [ 'module.exports = ('
        , intersperse(argNames, ',')
        , ') ->\n'
        , expression.source.split(/\n/).map(x => {
            return ' ' + x;
          }).join('\n')
        ].join('');

    return loadScript('compileExpression', source, env);
}



export function
intersperse<T>(arr: T[], obj: T): T[] {
    if (!arr.length) {
        return [];

    } else if (arr.length === 1) {
        return arr.slice(0);

    } else {
        var items = [arr[0]];
        for (var i = 1, len = arr.length; i < len; ++i) {
            items.push(obj, arr[i]);
        }

        return items;
    }
}


export function
resolveTerrainPosition
( state : State
, pos   : Storage.TerrainPosition
): TerrainPosition {
    var terrainEntity    = lookupEntity<Terrain>(state, pos.terrainEntityId)
      , pointsOfInterest = pointsOfInterestByName(state, pos.terrainEntityId, pos.pointOfInterestName)
      , poi              = randomListElement(state, pointsOfInterest);

    return terrainPositionAt(terrainEntity, <any>poi.position);
}


export function
randomListElement<T>(state: State, xs: T[]): T {
    return xs[randomIntBetween(state, 0, xs.length - 1)];
}
