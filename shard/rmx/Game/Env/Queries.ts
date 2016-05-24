
import { Vec3, vec3 } from 'gl-matrix';
import { _ } from 'underscore';

import { IEntity } from '../../Pure/Game';
import * as Storage from '../../Storage';
import { State } from '../../Game/State';
import { EntityId } from '../../Pure/Ids';
import { lookupResource, pointsOfInterestByName, forEachEntity } from '../../Game';
import { WorldObject, isAlive, distanceToEntity, reactionTowards, threatList } from '../../Game/Entities/WorldObject';
import { lookupEntity } from '../../Pure/Game';
import { TerrainPosition, TerrainTileInstance, Terrain,
    terrainPositionAt } from '../../Pure/Terrain';
import { checkParams, checkParamRange } from '../../Game/Env';
import { hasBehavior } from '../../Game/Behavior';
import { defineAccessor } from '../../Game/Env';


// Read-only access to the game state.

export function mk
(state: State) {
    var env: any = {};


    // Terrain
    // -------------------------------------------------------------------

    env.terrainGetPositionAt = function(id: EntityId, x, y, z): any {
        checkParams('terrainGetPositionAt', arguments, String, Number, Number, Number);
        var terrain = lookupEntity<Terrain>(state, id);
        return terrainPositionAt(terrain, vec3.fromValues(x, y, z));
    };

    env.terrainPositionAt = function(obj, x, y, z): any {
        if (obj instanceof TerrainTileInstance) {
            checkParams('terrainPositionAt', arguments, TerrainTileInstance, Number, Number);
            var pos = new TerrainPosition;
            if (obj.projectLocalCoordinates(pos, x, y)) {
                return pos;
            }

        } else if (_.isString(obj)) {
            checkParams('terrainPositionAt', arguments, String, Number, Number, Number);
            var terrain = lookupEntity<Terrain>(state, obj);
            return terrainPositionAt(terrain, vec3.fromValues(x, y, z));

        } else if (obj instanceof Terrain) {
            checkParams('terrainPositionAt', arguments, Terrain, Array);
            return terrainPositionAt(obj, x);

        } else {
            throw new Error("terrainPositionAt: Unknown source object");
        }
    };

    env.terrainGetTileByIndex = function(id: EntityId, index: number): any {
        checkParams('terrainGetTileByIndex', arguments, String, Number);
        var terrain = lookupEntity<Terrain>(state, id);
        return terrain.getTileInstanceByIndex(index);
    };

    env.terrainPositionGetTile = function(position: TerrainPosition): any {
        checkParams('terrainPositionGetTile', arguments, TerrainPosition);
        return position.tileInstanceId;
    };

    env.terrainPositionGetTerrain = function(position: TerrainPosition): any {
        checkParams('terrainPositionGetTile', arguments, TerrainPosition);
        return position.terrainId;
    };

    env.terrainTileGetPosition = function(tile: TerrainTileInstance, u: number, v: number): any {
        checkParams('terrainTileGetPosition', arguments, TerrainPosition);
        checkParamRange('terrainTileGetPosition', u, 'u', 0, 1);
        checkParamRange('terrainTileGetPosition', v, 'v', 0, 1);
        var pos = new TerrainPosition;
        if (tile.projectLocalCoordinates(pos, u, v)) {
            return pos;
        }
    };

    env.tileInstances = function(id: EntityId): any {
        checkParams('tileInstances', arguments, String);
        return _.values(lookupEntity<any>(state, id).tileInstances);
    };

    // FIXME: misleading name, returns a list of Vec3.
    env.pointOfInterestByName = function(terrainId: string, name: string): Vec3[] {
        checkParams('pointOfInterestByName', arguments, String, String);
        return pointsOfInterestByName(state, terrainId, name).map(poi => {
            return <any> poi.position;
        });
    };

    env.pointsOfInterestByName = function(terrainId: string, name: string): Storage.PointOfInterest[] {
        checkParams('pointsOfInterestByName', arguments, String, String);
        return pointsOfInterestByName(state, terrainId, name);
    };

    env.pointOfInterestById = function(terrainId: string, id: string): Vec3 {
        checkParams('pointOfInterestById', arguments, String, String);
        var terrain = lookupEntity<Terrain>(state, terrainId);
        var terrainDef =
            lookupResource<Storage.Terrain>(
                    state, terrain.resourceId, 'terrain');

        var poi = terrainDef.pointsOfInterest.filter(function(poi) {
            return poi.id == id;
        });

        if (poi.length > 0) {
            return <any> poi[0].position;
        } else {
            throw new Error("Unknown PointOfInterest in " + terrainId + ": " + id);
        }
    };



    // Party / Player / Entities
    // -------------------------------------------------------------------

    defineAccessor(env, 'parties', function() {
        return state.parties;
    });

    defineAccessor(env, 'players', function() {
        var players = [];
        state.parties.forEach(party => {
            party.players.forEach(player => {
                players.push(player);
            });
        });
        return players;
    });

    defineAccessor(env, 'worldGetUnits', function() {
        var units = [];
        forEachEntity(state, WorldObject, (entity) => {
            units.push(entity);
        });
        return units;
    });



    // WorldObject
    // -------------------------------------------------------------------

    function asWorldObject(func: string, entity: any): WorldObject {
        if (_.isString(entity)) {
            return lookupEntity<WorldObject>(state, entity);
        } else if (entity && entity.constructor && entity.constructor.name === 'WorldObject') {
            return <WorldObject> entity;
        } else {
            throw new Error(func + ': Expected WorldObject, but got ' + entity);
        }
    };

    env.unitGetName = function(entity: WorldObject): string {
        entity = asWorldObject('unitGetName', entity);
        return entity.name;
    };

    env.unitGetPosition = function(entity: WorldObject): any {
        entity = asWorldObject('unitGetPosition', entity);
        return entity.terrainPosition.clone();
    };

    env.terrain = function(entity: WorldObject): any {
        entity = asWorldObject('terrain', entity);
        return lookupEntity<any>(state, entity.terrainId);
    };

    env.target = function(entity: WorldObject): IEntity {
        entity = asWorldObject('target', entity);
        return lookupEntity(state, entity.targetId);
    };

    env.movementSpeed = function(entity: WorldObject): number {
        entity = asWorldObject('movementSpeed', entity);
        return entity.movementSpeed.value;
    };

    env.distanceBetween = function(x: WorldObject, y: WorldObject): number {
        x = asWorldObject('distanceBetween', x);
        y = asWorldObject('distanceBetween', y);
        return distanceToEntity(state, x, y);
    };

    env.spellBook = function(entity: WorldObject): string[] {
        entity = asWorldObject('spellBook', entity);
        return entity.spells;
    };

    env.scale = function(entity: WorldObject): number {
        entity = asWorldObject('scale', entity);
        return entity.scale.value;
    };

    env.health = function(entity: WorldObject): number {
        entity = asWorldObject('health', entity);
        return entity.health;
    };

    env.maxHealth = function(entity: WorldObject): number {
        entity = asWorldObject('maxHealth', entity);
        return entity.maxHealth.value;
    };

    env.power = function(entity: WorldObject): number {
        return asWorldObject('power', entity).power;
    };

    env.isAlive = function(entity: WorldObject): boolean {
        entity = asWorldObject('isAlive', entity);
        return isAlive(entity);
    };

    env.faction = function(entity: WorldObject): any {
        entity = asWorldObject('faction', entity);
        return entity.faction;
    };

    env.unitReaction = function(entity: WorldObject, other: WorldObject): any {
        entity = asWorldObject('unitReaction', entity);
        other  = asWorldObject('unitReaction', other);
        return reactionTowards(entity, other);
    };

    env.getThreat = function(entity: WorldObject, target: WorldObject): number {
        entity = asWorldObject('getThreat', entity);
        target = asWorldObject('getThreat', target);
        return entity.threatManager.getThreat(target.id);
    };

    env.getThreatList = function(entity: WorldObject): any[] {
        entity = asWorldObject('getThreatList', entity);
        return threatList(entity);
    };

    env.hasOwnBehavior = function(entity: WorldObject): boolean {
        entity = asWorldObject('hasOwnBehavior', entity);
        return hasBehavior(entity);
    };



    return env;
}
