
import { Projectile } from '../../Pure/Game';
import { WorldObject, distanceToSpellTarget } from '../../Game/Entities/WorldObject';


import { Spell, lookupEntity, destroyEntity, registerEntity, TargetInfo } from '../../Pure/Game';
import { Terrain, getWorldCoordinates } from '../../Pure/Terrain';
import { State } from '../../Game/State';
import { broadcastMessage, scheduleFuture, uniqueId } from '../../Game';
import { applySpellEffects } from '../../Game/Spell';
import * as Message from '../../Pure/Message';




// FIXME: Start the position in the center of the caster's model. For that
// we need the actual model or at least the size of the bounbing box so we
// can take its center.

export function
createProjectile
( state      : State
, spell      : Spell
, targetInfo : TargetInfo
, speed      : number
): void {
    var caster   = lookupEntity<WorldObject>(state, spell.casterId)
      , terrain  = lookupEntity<Terrain>(state, caster.terrainId)
      , distance = distanceToSpellTarget(state, caster, targetInfo.spellTarget);

    var projectile = new Projectile
        ( uniqueId(state)
        , spell
        , getWorldCoordinates(terrain, caster.terrainPosition)
        , targetInfo
        , state.currentTime
        , distance / speed
        );

    registerEntity(state, projectile);
    broadcastMessage(state, new Message.SMSG_CREATE_PROJECTILE(projectile));

    // Projectiles can not be destroyed, they always impact the target
    // after the calculated delay.
    scheduleFuture(state, projectile.delay, 'projectileImpact', state => {
        applySpellEffects(state, spell, targetInfo);
        destroyEntity(state, projectile.id);
    });
}
