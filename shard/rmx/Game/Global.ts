
import { topThreat, threatList,
    globalCooldownActive, triggerGlobalCooldown, setPositionType } from '../Game/Entities/WorldObject';
import { destroyEntity } from '../Pure/Game';
import { ShuffleBag } from '../Game/Random';
import * as Storage from '../Storage';
import { State } from '../Game/State';
import { attemptSpellcast } from '../Game/Spell';
import { EntityId } from '../Pure/Ids';
import { lookupResource, createTimer, createTerrain, intFromRange, debug } from '../Game';
import { WorldObject, spellCooldownActive, triggerSpellCooldown,
    createWorldObject } from '../Game/Entities/WorldObject';
import { Spell, SpellTarget } from '../Pure/Game';
import { spellInfo } from '../Game/SpellInfo';
import { TerrainPosition } from '../Pure/Terrain';
import { checkParams } from '../Game/Env';
import { SpellDamageInfo } from '../Game/Types';
import * as Env from '../Game/Env';


import * as Global from './Env/Global';
import * as Behavior from './Env/Behavior';
import * as Constructors from './Env/Constructors';
import * as Event from './Env/Event';
import * as PRNG from './Env/PRNG';
import * as Queries from './Env/Queries';
import * as SpellEffects from './Env/SpellEffects';

import { _ } from 'underscore';




export class OldGlobal {

    constructor(public state: State, origin, source) {
        var self = this;
        function define(name, options) {
          Object.defineProperty(self, name, options);
        }

        define('module', { value: {} });
        define('self', { value: source });

        define('shuffleBagCreate', {
          value: function() {
            return new ShuffleBag(state.prng);
          }
        });
        define('shuffleBagAdd', {
          value: function(bag, element) {
            checkParams('shuffleBagAdd', arguments, ShuffleBag, null);
            bag.add(element);
          }
        });
        define('shuffleBagNext', {
          value: function(bag) {
            checkParams('shuffleBagNext', arguments, ShuffleBag);
            return bag.next();
          }
        });
        define('debugMsg', {
          value: function(msg) {
            checkParams('debugMsg', arguments, String);
            console.log(msg);
          }
        });
        define('debug', {
          value: function(text) {
            checkParams('debug', arguments, String);
            debug(state, text);
          }
        });
        define('terrainCreate', {
            value: function(name: string, tag: EntityId) {
                checkParams('terrainCreate', arguments, String, String);
                return createTerrain(state, name, tag);
            }
        });
        define('terrainPositionSetFacing', {
            value: function(position: TerrainPosition, lookat) {
            var result;
            checkParams('terrainPositionSetFacing', arguments, TerrainPosition, TerrainPosition);
            result = position.clone();
            result.lookAt(lookat.getWorldCoordinates());
          }
        });
        define('worldDeleteUnit', {
            value: function(unit: WorldObject) {
            checkParams('worldDeleteUnit', arguments, WorldObject);
            if (unit) {
                destroyEntity(state, unit.id);
            }
          }
        });
        define('worldObjectTemplate', {
          value: function(creatureId) {
            checkParams('worldObjectTemplate', arguments, String);

            function ctor(template) {
                var ret: any = function(options) {
                    return ctor(_.extend({ creatureId: creatureId }, template, options));
                };

                ret.template = template;
                return ret;
            }

            var role0 = lookupResource<Storage.Creature>(state, creatureId, 'creature');

            var role;
            if (role0.behavior.objectId) {
                role = _.extend({}, role0, { behavior: role0.behavior.toString() });
            } else {
                role = _.extend({}, role0, { behavior: null });
            }

            return ctor(role);
          }
        });
        define('createWorldObject', {
          value: function(template) {
            checkParams('createWorldObject', arguments, Function);
            return createWorldObject(state, _.extend({
                faction: "Villains"
            }, template.template, { health: intFromRange(state, template.template.health) }));
          }
        });
        define('listFilter', {
          value: function(list, fn) {
            checkParams('listFilter', arguments, Array, Function);
            return list.filter(fn);
          }
        });
        define('listSelectRandom', {
          value: function(list) {
            checkParams('listSelectRandom', arguments, Array);
            return list[Math.floor(state.prng.random() * list.length)];
          }
        });
        define('forEach', {
          value: function(list, fn) {
            checkParams('forEach', arguments, Array, Function);
            list.forEach(function(x) {
              fn(x);
            });
          }
        });
        define('sortBy', {
          value: function(cmp, list) {
            checkParams('sortBy', arguments, Function, Array);
            return list.sort(cmp);
          }
        });
        define('unitGlobalCooldownActive', {
            value: function(unit: WorldObject) {
            checkParams('unitGlobalCooldownActive', arguments, WorldObject);
            return globalCooldownActive(state, unit);
          }
        });
        define('unitTriggerGlobalCooldown', {
            value: function(unit: WorldObject, duration) {
            checkParams('unitTriggerGlobalCooldown', arguments, WorldObject, Number);
            triggerGlobalCooldown(state, unit, duration);
          }
        });
        define('unitSpellCooldownActive', {
            value: function(unit: WorldObject, spellName) {
            checkParams('unitSpellCooldownActive', arguments, WorldObject, String);
            return spellCooldownActive(state, unit, spellName);
          }
        });
        define('unitTriggerSpellCooldown', {
            value: function(unit: WorldObject, spellName, duration) {
            checkParams('unitTriggerSpellCooldown', arguments, WorldObject, String, Number);
            triggerSpellCooldown(state, unit, spellName, duration);
          }
        });
        define('cast', {
            value: function(unit: WorldObject, spell, target) {
            if (_.isString(spell)) {
              checkParams('cast', arguments, WorldObject, String, WorldObject);
              attemptSpellcast(state, unit, new SpellTarget(target.id, undefined), spell);
            } else {
              if (target instanceof WorldObject) {
                checkParams('cast', arguments, WorldObject, Spell, WorldObject);
                attemptSpellcast(state, unit, new SpellTarget(target.id, undefined), spell.name);
              } else if (_.isArray(target)) {
                checkParams('cast', arguments, WorldObject, Spell, Array);
                attemptSpellcast(state, unit, new SpellTarget(target.id, undefined), spell.name);
              } else {
                throw new Error('Unknown arguments passed to `cast`');
              }
            }
          }
        });
        define('unitSetAnimationDeck', {
            value: function(unit: WorldObject, animation_deck) {
            checkParams('unitSetAnimationDeck', arguments, WorldObject, String);
            setPositionType(state, unit, animation_deck);
          }
        });
        define('topThreat', {
          get: function() {
            return topThreat(state, source);
          }
        });
        define('addThreat', {
            value: function(unit: WorldObject, target: WorldObject, threat) {
            checkParams('addThreat', arguments, WorldObject, WorldObject, Number);
            unit.threatManager.addThreat(target.id, threat);
          }
        });
        define('setTopThreat', {
            value: function(unit: WorldObject, target: WorldObject, factor) {
            checkParams('setTopThreat', arguments, WorldObject, WorldObject, Number);
            unit.threatManager.setTopThreat(target.id, factor);
          }
        });
        define('resetThreatList', {
            value: function(unit: WorldObject) {
                checkParams('resetThreatList', arguments, WorldObject);
                threatList(unit).forEach(entityId => {
                    unit.threatManager.removeFromThreatList(entityId);
                });
            }
        });
        define('stackCount', {
          get: function() {
            return origin.stackCount;
          }
        });
        define('spellName', {
          value: function(spellDamageInfo: SpellDamageInfo) {
            checkParams('spellName', arguments, SpellDamageInfo);
            return spellInfo(state, spellDamageInfo.spell.spellId).name;
          }
        });
        define('timerStart', {
          value: function(delay, repeats, callback) {
            checkParams('timerStart', arguments, Number, Number, Function);
            createTimer(state, delay, repeats, callback);
          }
        });


        Env.mix(this, Global.mk(state));
        Env.mix(this, PRNG.mk(state));
        Env.mix(this, Queries.mk(state));
        Env.mix(this, SpellEffects.mk(state, source, origin));
        Env.mix(this, Constructors.mk());

        // Required by behavior scripts.
        // ---------------------------------------------------------------

        Env.mix(this, Event.mk());
        Env.mix(this, Behavior.mk(state, source));
    }
}
