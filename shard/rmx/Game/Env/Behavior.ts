
import { topThreat } from '../../Game/Entities/WorldObject';
import { State } from '../../Game/State';
import { attemptSpellcast } from '../../Game/Spell';
import { EntityId } from '../../Pure/Ids';
import { WorldObject, setTargetId, walkTo, learnSpell, stopMoving,
    follow, say } from '../../Game/Entities/WorldObject';
import { SpellTarget, lookupEntity } from '../../Pure/Game';
import { Terrain, TerrainPosition, getWorldCoordinates } from '../../Pure/Terrain';
import { checkParams } from '../../Game/Env';
import { FollowMovementGenerator } from '../../Game/movement-generators/follow';

import { _ } from 'underscore';


export function mk
(state: State, entity: WorldObject) {
    var env: any = {};

    env.say = function(message: string): void {
        say(state, entity, message);
    };


    env.getTarget = function(): EntityId {
        return topThreat(state, entity) || entity.targetId;
    };


    env.follow = function(targetId: EntityId, distance: number): string {
        checkParams('behavior::follow', arguments, String, Number);
        var target = lookupEntity<WorldObject>(state, targetId);

        // Avoid changing the movement generator if we're already following
        // the target.
        var mg = entity.movementGenerator;
        if (mg && mg instanceof FollowMovementGenerator) {
            var fmg = <FollowMovementGenerator> mg;
            if (fmg.target === target) {
                return;
            }
        }

        follow(state, entity, target, distance);
    };


    env.walkTo = function(position: TerrainPosition): void {
        checkParams('behavior::walkTo', arguments, TerrainPosition);
        walkTo(state, entity, position, 0);
    };


    env.stopMoving = function(): void {
        checkParams('behavior::stopMoving', arguments);
        stopMoving(state, entity);
    };


    env.attemptSpellcast = function(spellId: string, spellTarget: SpellTarget): void {
        checkParams('behavior::attemptSpellcast', arguments, String, SpellTarget);
        attemptSpellcast(state, entity, spellTarget, spellId);
    };


    env.learnSpell = function(spellId: string): void {
        learnSpell(state, entity, spellId);
    };


    env.toSpellTarget = function(obj: any): SpellTarget {
        if (obj instanceof SpellTarget) {
            return obj;

        } else if (obj instanceof TerrainPosition) {
            var p        = <TerrainPosition> obj
              , terrain  = lookupEntity<Terrain>(state, p.terrainId)
              , location = getWorldCoordinates(terrain, p);

            // FIXME: Remove the cast once SpellTarget uses Vec3.
            return new SpellTarget(null, <any>location);

        } else if (obj instanceof String || _.isString(obj)) {
            // TODO: Investigate why the instanceof check doesn't work,
            // and why we must use _.isString instead.

            return new SpellTarget(<string>obj, null);

        } else if (obj instanceof WorldObject) {
            return new SpellTarget(obj.id, null);

        } else {
            throw new Error(
                [ 'behavior::toSpellTarget: '
                , 'Could not convert object "'
                , '' + obj
                , '" to SpellTarget'
                ].join('')
            );
        }
    };

    env.selectTarget = function(entityId: string): void {
        if (_.isString(entityId)) {
            checkParams('behavior::selectTarget', arguments, String);
            setTargetId(state, entity, entityId);

        } else {
            // This form is deprecated!
            checkParams('behavior::selectTarget', arguments, WorldObject);
            setTargetId(state, entity, (<any>entityId).id);
        }
    };



    return env;
}
