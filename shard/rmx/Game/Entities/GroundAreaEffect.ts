
import { WorldObject } from '../../Game/Entities/WorldObject';


import { EntityId } from '../../Pure/Ids';
import { lookupEntity, destroyEntity, registerEntity, withEntity } from '../../Pure/Game';
import { Terrain, TerrainPosition, getWorldCoordinates } from '../../Pure/Terrain';
import { State } from '../../Game/State';
import { broadcastMessage, stdFormula, scheduleFuture, forEachEntity,
    uniqueId } from '../../Game';
import { SMSG_CREATE_GROUNDAREAEFFECT } from '../../Game/Messages';
import { formulaModifiers, distanceToLocation } from '../../Game/Entities/WorldObject';
import { removeAura, applyAura } from '../../Game/Aura';
import { toString } from '../../Pure/String';



export class GroundAreaEffect {

    affectedEntities : Map<EntityId, string>;
    // ^ Map from aura holder to slot.

    constructor
      ( public id              : EntityId
      , public casterId        : EntityId
      , public createdAt       : number
      , public terrainPosition : TerrainPosition
      , public duration        : number
      , public radius          : number
      , public auraId          : string
      ) {
        this.affectedEntities = new Map<EntityId, string>();
    }

    toString() {
        return toString(this, 'GroundAreaEffect', 'id');
    }
}



export function
createGroundAreaEffect
  ( state           : State
  , caster          : WorldObject
  , terrainPosition : TerrainPosition
  , duration0       : number
  , radius0         : number
  , auraId          : string
  ) {
    var id  = uniqueId(state);

    var durationModifiers = formulaModifiers(state, caster, 'duration')
      , duration          = stdFormula.apply(duration0, durationModifiers);

    var radiusModifiers   = formulaModifiers(state, caster, 'radius')
      , radius            = stdFormula.apply(radius0, radiusModifiers);

    var gae = new GroundAreaEffect
        ( id
        , caster.id
        , state.currentTime
        , terrainPosition
        , duration
        , radius
        , auraId
        );

    registerEntity(state, gae);
    broadcastMessage(state, SMSG_CREATE_GROUNDAREAEFFECT(gae));

    scheduleFuture(state, gae.duration, 'removeGroundAreaEffect', state => {
        removeGroundAreaEffect(state, id);
    });

    scheduleTick(state, id);
    tick(state, gae);
}

export function
removeGroundAreaEffect(state: State, id: EntityId) {
    withEntity<GroundAreaEffect>(state, id, GroundAreaEffect, gae => {
        forEachEntity<WorldObject>(state, WorldObject, x => {
            removeAffectedEntity(state, gae, x.id);
        });

        destroyEntity(state, id);
    });
}


function
removeAffectedEntity(state: State, gae: GroundAreaEffect, id: EntityId) {
    var slot = gae.affectedEntities.get(id);
    if (slot) {
        gae.affectedEntities.delete(id);

        withEntity<WorldObject>(state, id, WorldObject, entity => {
            removeAura(state, entity, slot);
        });
    }
}


function
isWithinRange(state: State, gae: GroundAreaEffect, x: WorldObject): boolean {
    if (x.terrainId !== gae.terrainPosition.terrainId) {
        // Different terrain is always out of range.
        return false;

    } else {
        var terrain = lookupEntity<Terrain>(state, gae.terrainPosition.terrainId);
        if (terrain) {
            // Within range if the WorldObject is within the GAE radius.
            var center = getWorldCoordinates(terrain, gae.terrainPosition);
            return distanceToLocation(state, x, center) <= gae.radius;

        } else {
            // The terrain in which the GAE is placed doesn't exist?
            // Strange. Anyways, nothing is in its range now.
            return false;
        }
    }
}


function
tick(state: State, gae: GroundAreaEffect) {
    scheduleTick(state, gae.id);

    var caster = lookupEntity<WorldObject>(state, gae.casterId);

    forEachEntity<WorldObject>(state, WorldObject, x => {
        if (isWithinRange(state, gae, x)) {
            if (!gae.affectedEntities.has(x.id)) {
                // FIXME: 'caster' may be undefined.
                var slot = applyAura(state, caster, x, gae.auraId);
                gae.affectedEntities.set(x.id, slot);
            }
        } else {
            removeAffectedEntity(state, gae, x.id);
        }
    });
}


function
scheduleTick(state: State, id: EntityId) {
    scheduleFuture(state, 0.1, 'tickGroundAreaEffect', state => {
        withEntity<GroundAreaEffect>(state, id, GroundAreaEffect, gae => {
            tick(state, gae);
        });
    });
}
