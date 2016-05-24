/// <reference path="../../Pure/Terrain.ts" />
/// <reference path="../../Pure/Game.ts" />

/// <reference path="../systems.ts" />
/// <reference path="../Entities/WorldObject.ts" />


module rmx.Game.Systems {

    import lookupEntity = rmx.Pure.lookupEntity;


    export class Projectile implements rmx.Game.ISystem {

        update(state: rmx.Game.State, now: number, dt: number): void {
            state.entities.forEach(entity => {
                if (entity instanceof rmx.Pure.Projectile) {
                    var projectile = <rmx.Pure.Projectile> entity;

                    // FIXME: This assumes the targetInfo spell target is
                    // a WorldObject.
                    var target = lookupEntity<WorldObject>(state, projectile.targetInfo.spellTarget.entityId);

                    var direction = vec3.sub(vec3.create(), target.position, projectile.position);
                    var distance  = vec3.length(direction);
                    if (distance > .1) {
                        var delay = (projectile.createdAt + projectile.delay) - now;
                        if (delay > 0) {
                            var speed = distance / delay;
                            vec3.normalize(direction, direction);
                            vec3.scale(direction, direction, Math.min(speed * dt, distance));

                            vec3.add(projectile.position, projectile.position, direction);
                        } else {
                            vec3.copy(projectile.position, target.position);
                        }
                    }
                }
            });
        }
    }
}
