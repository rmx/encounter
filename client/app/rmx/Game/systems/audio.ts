/// <reference path="../../Pure/Terrain.ts" />
/// <reference path="../../Core/Audio.ts" />

/// <reference path="../../../entry.ts" />
/// <reference path="../systems.ts" />


module rmx.Game.Systems {

    import Terrain = rmx.Pure.Terrain;


    export class Audio implements rmx.Game.ISystem {

        handle : rmx.Core.Audio.Handle;

        constructor(public client: Client) {
            this.handle = rmx.Core.Audio.allocateHandle(rmx.app.audio);
        }


        // ISystem
        // ----------------------------------------------------------------

        update(state: rmx.Game.State, now: number, dt: number): void {
            rmx.Core.Audio.startNextGeneration(this.handle);

            var prefs = rmx.data.preferences;
            if (prefs.audio.enabled === false) {
                return;
            }

            state.entities.forEach((entity: any) => {
                if (entity instanceof WorldObject) {
                    worldObjectSounds(this, entity, now, dt);

                    if (entity === this.client.controlledEntity) {
                        rmx.Core.Audio.relocateListener(this.handle, entity.position);
                    }

                } else if (entity instanceof Terrain && isActiveTerrain(state, entity) && state.stage == rmx.Pure.Stage.Running) {
                    var terrain = <Terrain> entity
                      , res     = rmx.Core.resourceContent<rmx.Storage.Terrain>(terrain.resourceId).get(null);

                    if (res && res.sound.toString()) {
                        var node = rmx.Core.Audio.placeSound
                            ( this.handle
                            , entity.id + '/ambientSound'
                            , res.sound.toString()
                            , false
                            , null
                            );

                        if (node) {
                            node.volume.gain.value = .3;
                        }
                    }
                }
            });


            // Spellcast Finish
            eventsInFrame(state, now, dt).filter(ae => {
                return ae.event instanceof rmx.Game.SpellcastFinish;

            }).forEach(atm => {
                var sf = <rmx.Game.SpellcastFinish> atm.event
                  , wo = rmx.Pure.lookupEntity<WorldObject>(state, sf.casterId);

                if (wo) {
                    var res = rmx.Core.resourceContent<rmx.Storage.Spell>(sf.spell.spellId).get(null);
                    if (res) {
                        placeSensuousSound
                            ( this.handle
                            , res.sensousEffects.castEnd
                            , wo.id + '/' + sf.spell.id + '/castEnd'
                            , true
                            , wo.position
                            );
                    }
                }
            });


            // Spell hit (CombatEvent with Spell source)
            eventsInFrame(state, now, dt).filter(ae => {
                return ae.event instanceof rmx.Pure.Game.CombatEvent;

            }).forEach(atm => {
                var ce     = <rmx.Pure.Game.CombatEvent> atm.event
                  , source = ce.source
                  , wo     = rmx.Pure.lookupEntity<WorldObject>(state, ce.entityId);

                if (wo && source instanceof rmx.Pure.Spell) {
                    var res = rmx.Core.resourceContent<rmx.Storage.Spell>(source.spellId).get(null);
                    if (res && res.sensousEffects.spellHit.soundId) {
                        placeSensuousSound
                            ( this.handle
                            , res.sensousEffects.spellHit
                            , wo.id + '/' + source.id + '/spellHit'
                            , true
                            , wo.position
                            );
                    }
                }
            });


            rmx.Core.Audio.cleanupHandle(this.handle);
        }
    }


    function
    worldObjectSounds
    ( system : Audio
    , entity : WorldObject
    , now    : number
    , dt     : number
    ): void {
        if (entity.spell) {
            var spell = rmx.Core.resourceContent<rmx.Storage.Spell>(entity.spell.spellId).get(null);
            if (spell) {
                var meta = spell.sensousEffects;

                if (timeWithin(entity.spell.startedAt, now, dt)) {
                    placeSensuousSound
                        ( system.handle
                        , meta.castBegin
                        , entity.id + '/' + entity.spell.id + '/castBegin'
                        , true
                        , entity.position
                        );
                }

                placeSensuousSound
                    ( system.handle
                    , meta.casting
                    , entity.id + '/' + entity.spell.id + '/casting'
                    , false
                    , entity.position
                    );
            }
        }

        entity.auraList.forEach(x => {

            var aura = rmx.Core.resourceContent<rmx.Storage.Aura>(x.auraId).get(null);
            if (aura) {
                var meta = aura.sensousEffects;

                if (timeWithin(x.createdAt, now, dt)) {
                    placeSensuousSound
                        ( system.handle
                        , meta.auraStart
                        , entity.id + '/' + x.slot + '/auraStart'
                        , true
                        , entity.position
                        );
                }

                //FIXME: handle other aura events
                //auraEnd
                //auraTick

                placeSensuousSound
                    ( system.handle
                    , meta.auraSteady
                    , entity.id + '/' + x.slot + '/auraSteady'
                    , false
                    , entity.position
                    );
            }
        });
    }


    function
    placeSensuousSound
    ( handle  : rmx.Core.Audio.Handle
    , ses     : rmx.Storage.SensousEffectState
    , soundId : string
    , oneshot : boolean
    , position : Vec3
    ): void {
        if (ses.soundId) {
            rmx.Core.Audio.placeSound
                ( handle
                , soundId
                , ses.soundId
                , oneshot
                , position
                );
        }
    }
}
