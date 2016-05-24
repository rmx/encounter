/// <reference path="../../Pure/Terrain.ts" />
/// <reference path="../../Pure/Game.ts" />

/// <reference path="../systems.ts" />
/// <reference path="../Entities/WorldObject.ts" />


module rmx.Game.Systems {

    // This inserts 'AuraTickMessage' into the log if there was a tick in the
    // current frame.

    export class AuraTick implements rmx.Game.ISystem {

        constructor
            ( public client : Client
            ) {}

        update(state: rmx.Game.State, now: number, dt: number): void {
            state.entities.forEach(entity => {
                if (entity instanceof rmx.Game.WorldObject) {
                    entity.auraList.forEach(aura => {
                        if (aura.tickInterval) {
                            var t = aura.createdAt - this.client.serverTime + this.client.localTime;
                            while (t < now) {
                                t += aura.tickInterval;

                                if (timeWithin(t, now, dt)) {
                                    pushAnnotatedEvent(state, new AuraTickMessage(aura));
                                }
                            }
                        }
                    });
                }
            });
        }
    }
}
