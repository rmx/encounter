/// <reference path="../../Pure/Ids.ts" />
/// <reference path="../../Pure/Math.ts" />

/// <reference path="../../Core/Input.ts" />
/// <reference path="../../Core/Bindings.ts" />
/// <reference path="../../Core/InputSource.ts" />

/// <reference path="../../Core/WebGL/Pure.ts" />

/// <reference path="../Entities/WorldObject.ts" />
/// <reference path="../Client.ts" />
/// <reference path="../systems.ts" />
/// <reference path="../../../hotkey.ts" />
/// <reference path="../../time.ts" />


module rmx.Game.Systems {

    import EntityId     = rmx.Pure.EntityId;
    import InputSource  = rmx.Core.InputSource;
    import lookupEntity = rmx.Pure.lookupEntity;


    export function
    asWorldObject(
        state: rmx.Game.State, spellTarget: rmx.Pure.SpellTarget): {position: Vec3} {

        if (spellTarget.entityId != null) {
            return lookupEntity<WorldObject>(state, spellTarget.entityId);
        } else if (spellTarget.location != null) {
            return {
                position: vec3.clone(spellTarget.location)
            };
        } else {
            return null;
        }
    }

    export function
    asRenderObject(
        state: rmx.Game.State, scene: rmx.WebGL.Scene,
        spellTarget: rmx.Pure.SpellTarget): WebGL.RenderObject {

        if (spellTarget.entityId != null) {
            return WebGL.lookupRenderObject(
                scene, <string>(spellTarget.entityId));
        } else if (spellTarget.location != null) {
            var obj = new WebGL.RenderObject("FIXME: UNIQUE ID");
            obj.position = vec3.clone(spellTarget.location);
            return obj;
        } else {
            return null;
        }
    }


    export function
    applyCameraTo(client: Client, camera: THREE.PerspectiveCamera, entity: WorldObject): void {
        var cam = client.camera;
        if (entity.position) {
            var epos = entity.position;
            var cpos = cam.positionRelativeTo(entity.position);

            camera.position.set(cpos[0], cpos[1], cpos[2]);

            var target = new THREE.Vector3(epos[0], epos[1], epos[2] + 1.5);
            camera.lookAt(target);
        }
    }

    export function
    threeVectorToGlMatrix(vec: THREE.Vector3): Vec3 {
        return vec3.fromValues(vec.x, vec.y, vec.z);
    }

    export function
    tileModel(tileId: string): Computation<rmx.Storage.Model> {
        return rmx.data.findById<rmx.Storage.Tile>(tileId).fmap(entity => {
            return entity.content.model;
        });
    }

    export function
    modelModel(modelId: string): Computation<rmx.Storage.Model> {
        return rmx.data.findById<rmx.Storage.Model>(modelId).fmap(entity => {
            return entity.content;
        });
    }

    export class Renderer implements rmx.Game.ISystem {

                scene       : WebGL.Scene;
        private inputSource : InputSource;


        // These need to be cached per-renderer (WebGL context to be precise).
        targetCircleTexture = undefined;
        indicatorTexture    = undefined;
        groundTargetTexture = undefined;

        // Colorful factions (we serve only 5 different factions,
        // that should be more than enough)
        private factionColors = [ 0x209678, 0x8d34df, 0xe257b1, 0xb6894d,  0x99cb2f];
        private factionColorAssignment = {};


        constructor
          ( private client : rmx.Game.Client
          ) {
            this.scene       = new WebGL.Scene;
            this.inputSource = new InputSource(this.client.input, this.client.bindings, this.scene.domElement);

            // Preload assets
            groundTargetTexture(this);
            targetCircleTexture(this);
            indicatorTexture(this);
        }


        appendTo(el: HTMLElement): void {
            this.scene.appendTo(el);
            this.inputSource.bindEvents();
        }

        removeFrom(el: HTMLElement): void {
            this.scene.removeFrom(el);
            this.inputSource.unbindEvents();
        }


        update(state: rmx.Game.State, now: number, dt: number): void {
            WebGL.startNextGeneration(this.scene);

            var controlledEntity = this.client.controlledEntity;
            if (controlledEntity) {
                applyCameraTo(this.client, this.scene.camera, controlledEntity);
            }

            state.entities.forEach(entity => {
                if (entity instanceof WorldObject) {
                    this.renderWorldObject(state, <WorldObject> entity, now, dt);
                } else if (entity instanceof rmx.Pure.Terrain) {
                    this.renderTerrain(state, <rmx.Pure.Terrain> entity);
                } else if (entity instanceof rmx.Pure.GroundAreaEffect) {
                    rmx.Core.WebGL.renderGroundAreaEffect(this.scene, <rmx.Pure.GroundAreaEffect> entity);
                } else if (entity instanceof rmx.Pure.Projectile) {
                    rmx.Core.WebGL.renderProjectile(this.scene, <rmx.Pure.Projectile> entity);
                }
            });

            this.updateSensuous(state, now, dt);

            // The ground target texture.
            function tileInstanceFilter(obj: THREE.Object3D): boolean {
                var owner = WebGL.toRenderObject(obj);
                return owner && !!owner.id.match(/\/ti$/);
            }

            var intersection = WebGL.intersectionAtPointer(this.scene, tileInstanceFilter);
            if (this.client.selectedAction && intersection) {

                var player         = rmx.Pure.lookupPlayer(state, rmx.data.session.accountId)
                  , targetPosition = threeVectorToGlMatrix(intersection.point)
                  , color          = groundTargetColor(state, player.entityId, targetPosition, this.client.selectedAction);

                var gtt    = mkGroundTargetTexture(this, this.scene, color);
                var radius = actionRadius(this.client.selectedAction) || 1;

                gtt.position = targetPosition;
                gtt.position[2] += 0.07;

                gtt.scale = vec3.fromValues(radius, radius, 1);
            }
        }

        factionColor( targetFaction : string): any {

            if(targetFaction in this.factionColorAssignment) {
                return this.factionColorAssignment[targetFaction];
            } else {
                if(this.factionColors.length == 0) {
                    return 0x0b8090;
                }

                var target = this.factionColors[0];
                this.factionColorAssignment[targetFaction] = target;
                this.factionColors.shift();
                return target;
            }
        }

        renderWorldObject(state: rmx.Game.State, entity: WorldObject, now: number, dt: number): void {
            var ro = WebGL.renderObject(this.scene, <string>entity.id);

            ro.model = modelModel(entity.appearance.modelId).get(null);

            var scale = vec3.fromValues(entity.scale, entity.scale, entity.scale);
            WebGL.updateModelMatrix(ro, entity.position, entity.terrainPosition.heading, undefined, scale);

            var animationName = "idle";
            if(ro.model != null)
                animationName = entityAnimationName(entity, ro.model, now, dt);
            if (!ro.animation || ro.animation.name != animationName) {
                ro.animation = new rmx.WebGL.Animation
                    ( animationName
                    , now
                    , animationName != 'death'
                    , isEntityFrozen(entity)
                    , 0
                    );
            }


            // Place the target circle under the WorldObject if it's the one currently
            // targetted by the player.

            var player       = rmx.Pure.lookupPlayer(state, rmx.data.session.accountId)
              , playerEntity = rmx.Pure.lookupEntity<WorldObject>(state, player.entityId);

            if (entity && playerEntity) {

                var factionColor = this.factionColor(entity.faction);
                var color = targetCircleColor(state, playerEntity.id, entity.id, factionColor);

                if(playerEntity.id != entity.id) {
                    renderGroundIndicatorTexture(
                        this, this.scene, entity.id, entity.position, color, entity.scale);
                }

                if (playerEntity && playerEntity.targetId === entity.id) {
                    renderGroundTargetTexture(
                        this, this.scene, entity.position, color, entity.scale);
                }
            }


            if (this.client.showMovePaths && entity.movePath) {
                rmx.Core.WebGL.renderPureMovePath(this.scene, entity.id, entity.movePath);
            }
        }

        renderTerrain(state: State, terrain: rmx.Pure.Terrain): void {
            if (isActiveTerrain(state, terrain)) {
                var storageTerrain = rmx.Core.resourceContent<rmx.Storage.Terrain>(terrain.resourceId).get(null);
                if (storageTerrain) {
                    renderSkyboxId(this.scene, storageTerrain.skybox.toString());
                    this.scene.setAmbientLight(storageTerrain.ambientLight);
                    this.scene.setSunLight(storageTerrain.sunLight);
                    this.scene.setSunPos(storageTerrain.sunPos);
                    this.scene.setShadowDarkness(storageTerrain.shadowDarkness);
                }

                rmx.Core.WebGL.renderPureTerrain(this.scene, terrain);

                if (this.client.showNavmesh) {
                    rmx.Core.WebGL.renderPureHalfEdgeMesh(this.scene, terrain.id, terrain.navmesh, 0);
                }
            }
        }

        // handle particle effects
        private updateSensuous(state: rmx.Game.State, now: number, dt: number): void {

            // Aura tick (particle effects)
            eventsInFrame(state, now, dt).filter(ae => {
                return ae.event instanceof AuraTickMessage;

            }).forEach(atm => {
                var aura = (<AuraTickMessage>atm.event).aura
                  , wo   = lookupEntity<WorldObject>(state, aura.holderId);

                if (wo) {
                    var self = WebGL.lookupRenderObject(
                        this.scene, <string>(wo.id));

                    var nem: rmx.WebGL.NameEntityMap =
                        { self : self, target : self };

                    var ac = rmx.Core.resourceContent<rmx.Storage.Aura>(aura.auraId).get(null);
                    if (ac) {
                        var meta = ac.sensousEffects;
                        startParticleEffect(this.scene,
                            meta.auraTick.particleId, nem,
                            wo.id + '/' + aura.slot + '/auraTick/' + atm.time,
                            true);
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
                        var target = asRenderObject(
                            state, this.scene, sf.spell.spellTarget);

                        var self = WebGL.lookupRenderObject(
                            this.scene, <string>(wo.id));

                        startParticleEffect
                            ( this.scene
                            , res.sensousEffects.castEnd.particleId
                            , { self: self, target: target }
                            , wo.id + '/' + sf.spell.id + '/castEnd'
                            , true
                            );
                    }
                }
            });

            state.entities.forEach(entity => {
                if (entity instanceof WorldObject) {
                    var wo = <WorldObject> entity;

                    if(wo.spell) {

                        rmx.Core.resourceContent<rmx.Storage.Spell>(wo.spell.spellId).fmap(spell => {
                            // first we build the named entity map
                            var target = asRenderObject(
                                state, this.scene, wo.spell.spellTarget);

                            var self = WebGL.lookupRenderObject(
                                this.scene, <string>(wo.id));

                            var nem: rmx.WebGL.NameEntityMap =
                                { self : self, target : target };


                            var meta = spell.sensousEffects;

                            // CAST BEGIN
                            if (timeWithin(wo.spell.startedAt, now, dt)) {
                                startParticleEffect(this.scene,
                                    meta.castBegin.particleId, nem,
                                    wo.id + '/' + wo.spell.id + '/castBegin',
                                    true);
                            }

                            // CASTING
                            if (wo.spell.startedAt < now - dt &&
                                wo.spell.startedAt + wo.spell.castTime > now) {

                                startParticleEffect(this.scene,
                                    meta.casting.particleId, nem,
                                    wo.id + '/' + wo.spell.id + '/casting',
                                    false);
                            }

                            //TODO
                            // CASTING PULSE
                            // SPELL HIT

                        }).get(null);
                    }

                    wo.auraList.forEach(x => {

                        var self = WebGL.lookupRenderObject(
                            this.scene, <string>(wo.id));

                        var nem: rmx.WebGL.NameEntityMap =
                            { self : self, target : self };

                        var aura = rmx.Core.resourceContent<rmx.Storage.Aura>(x.auraId).get(null);
                        if (aura) {
                            var meta = aura.sensousEffects;

                            if (timeWithin(x.createdAt, now, dt)) {
                                startParticleEffect(this.scene,
                                    meta.auraStart.particleId, nem,
                                    wo.id + '/' + x.slot + '/auraStart',
                                    true);
                            }

                            //FIXME: handle other aura events
                            //auraEnd

                            startParticleEffect(this.scene,
                                meta.auraSteady.particleId, nem,
                                wo.id + '/' + x.slot + '/auraSteady',
                                false);
                        }
                    });
                }
            });
        }
    }


    function
    startParticleEffect
    ( scene            : rmx.WebGL.Scene

    , particleEffectId : string
      // ^ May be null. If not provided, the particle effect will not be
      // started.

    , nem              : rmx.WebGL.NameEntityMap
    , id               : string
    , oneshot          : boolean
    ): void {
        var prefs = rmx.data.preferences;

        if (prefs.graphics.particleEffects.enabled && particleEffectId) {
            var obj = rmx.data.findById<rmx.Storage.ParticleEffect>(particleEffectId).get(null);
            if (obj) {
                WebGL.particleEffectObject(scene, obj.content, nem, id, oneshot);
            }
        }
    }


    export function offsetX(event: any): number {
        if (typeof event.offsetX !== 'undefined') {
            return event.offsetX;
        } else {
            return event.clientX - event.target.getBoundingClientRect().left;
        }
    }

    export function offsetY(event: any): number {
        if (typeof event.offsetY !== 'undefined') {
            return event.offsetY;
        } else {
            return event.clientY - event.target.getBoundingClientRect().top;
        }
    }

    export function eventX(event: any): number {
        return (offsetX(event) / event.target.clientWidth) * 2 - 1;
    }

    export function eventY(event: any): number {
        return -(offsetY(event) / event.target.clientHeight) * 2 + 1;
    }

    function groundTargetColor
    ( state          : State
    , casterEntityId : EntityId
    , targetPos      : Vec3
    , action         : rmx.Game.Action
    ): number {

        var spellRange = rmx.Storage.Range.mk(0, 0);
        if (action instanceof rmx.Game.SpellAction) {
            spellRange=
                rmx.Core.resourceContentReference<rmx.Storage.Spell>(action.spellRef).fmap(spell => {
                    return spell.range;
                }).get(undefined);
        }

        var entity = rmx.Pure.lookupEntity<WorldObject>(state, casterEntityId);
        if (entity && entity instanceof WorldObject) {

            var distance = vec3.dist(entity.position, targetPos);
            if (spellRange.min <= distance && distance <= spellRange.max) {
                // Green color if the target is in range
                return 0x30d035;
            } else {
                // Red color if the target is too far away
                return 0xff0000;
            }

        } else {
            // Caster is not a WorldObject? WTF?
            return 0xffffff;
        }
    }


    // Load the ground target texture image on first acess, then cache it.
    function groundTargetTexture(renderer: Renderer) {
        if (renderer.groundTargetTexture === undefined) {
            renderer.groundTargetTexture = THREE.ImageUtils.loadTexture(rmx.assets.groundTargetTexture);
        }

        return renderer.groundTargetTexture;
    }


    function mkGroundTargetMesh(renderer: Renderer, color : number): THREE.Mesh {
        var geometry = new THREE.PlaneBufferGeometry(2, 2, 1, 1);
        var material = new THREE.MeshBasicMaterial({ map: groundTargetTexture(renderer) });
        material.transparent = true;
        material.alphaTest = 1 / 255;
        material.color.setHex(color);

        var mesh = new THREE.Mesh(geometry, material);
        mesh.scale.set(1, 1, 1);
        return mesh;
    }

    function
    mkGroundTargetTexture
    ( renderer : Renderer
    , scene    : WebGL.Scene
    , color    : number
    ) : WebGL.RenderObject {
        var ro = WebGL.renderObject(scene, 'groundTargetTexture');

        ro.castShadow = false;

        ro.position   = vec3.fromValues(0,0,0);
        ro.rotation   = vec3.fromValues(0,0,0);
        ro.selectable = false;

        WebGL.updateVisual(scene, ro, color, () => {
            return mkGroundTargetMesh(renderer, color);
        });

        return ro;
    }

    export function
    isActiveTerrain(state: State, terrain: rmx.Pure.Terrain): boolean {
        var player = rmx.Pure.lookupPlayer(state, rmx.data.session.accountId);
        if (player) {
            var entityId = player.entityId
              , entity   = rmx.Pure.lookupEntity<WorldObject>(state, entityId);

            if (entity) {
                return entity.terrainPosition.terrainId == terrain.id;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }


    function
    isInSameParty
    ( state          : State
    , playerEntityId : EntityId
    , targetEntityId : EntityId
    ): boolean {
        var playerParty = null
          , targetParty = null;

        state.parties.forEach(party => {
            party.players.forEach(player => {
                if (player.entityId === playerEntityId) {
                    playerParty = party;
                }
                if (player.entityId === targetEntityId) {
                    targetParty = party;
                }
            });
        });

        return playerParty && playerParty === targetParty;
    }

    function
    targetCircleColor
    ( state          : State
    , playerEntityId : EntityId
    , targetEntityId : EntityId
    , factionColor   : number
    ): number {
        var entity = rmx.Pure.lookupEntity<WorldObject>(state, targetEntityId);
        if (entity && entity instanceof WorldObject) {

            if (entity.health === 0) {
                // Grey color if the entity is dead.
                return 0xbbbbbb;

            } else if (playerEntityId == targetEntityId) {
                // If the target is the player, use a violet color.
                return 0x6000ff;

            } else if (isInSameParty(state, playerEntityId, targetEntityId)) {
                // Targets in the same party get a blue color.
                return 0x0000ff;

            } else {
                // Enemies get faction color.
                return factionColor;
            }

        } else {
            // Target is not a WorldObject? WTF?
            return 0xffff00;
        }
    }

    function
    renderSkyboxId
    ( scene    : WebGL.Scene
    , skyboxId : string
    ): void {
        var skybox = rmx.data.findById<rmx.Storage.Skybox>(skyboxId).fmap(obj => {
            return obj.content;
        }).get(null);

        if (skybox) {
            rmx.Core.WebGL.renderStorageSkybox(scene, skyboxId, skybox);
        }
    }


    function
    renderGroundTargetTexture
    ( renderer : Renderer
    , scene    : rmx.WebGL.Scene
    , position : Vec3
    , color    : number
    , scale    : number
    ): void {
        var ro  = WebGL.renderObject(scene, 'targetCircle')
          , pos = vec3.add(vec3.create(), position, vec3.fromValues(0, 0, 0.05));

        ro.castShadow = false;

        WebGL.updateModelMatrix(ro, pos, 0, undefined, vec3.fromValues(scale, scale,1));
        WebGL.updateVisual(scene, ro, color, () => {
            return mkTargetCircleMesh(renderer, color, targetCircleTexture(renderer));
        });
    }


    function targetCircleTexture(renderer: Renderer) {
        if (renderer.targetCircleTexture === undefined) {
            renderer.targetCircleTexture = THREE.ImageUtils.loadTexture(rmx.assets.targetCircle);
        }

        return renderer.targetCircleTexture;
    }


    function
    mkTargetCircleMesh(renderer: Renderer, color: number, texture: THREE.Texture): THREE.Mesh {
        var geometry = new THREE.PlaneBufferGeometry(2, 2, 1, 1);
        var material = new THREE.MeshBasicMaterial({ map: texture });
        material.transparent = true;
        material.alphaTest = 1 / 255;
        material.color.setHex(color);

        return new THREE.Mesh(geometry, material);
    }


    function
    renderGroundIndicatorTexture
    ( renderer : Renderer
    , scene    : rmx.WebGL.Scene
    , id       : EntityId
    , position : Vec3
    , color    : number
    , scale    : number
    ): void {
        var ro  = WebGL.renderObject(scene, 'indicatorCircle_' + id)
          , pos = vec3.add(vec3.create(), position, vec3.fromValues(0, 0, 0.05));

        ro.castShadow = false;

        WebGL.updateModelMatrix(ro, pos, 0, undefined, vec3.fromValues(scale, scale,1));
        WebGL.updateVisual(scene, ro, color, () => {
            return mkTargetCircleMesh(renderer, color, indicatorTexture(renderer));
        });
    }


    function indicatorTexture(renderer: Renderer) {
        if (renderer.indicatorTexture === undefined) {
            renderer.indicatorTexture =
                THREE.ImageUtils.loadTexture(rmx.assets.indicatorCircle);
        }

        return renderer.indicatorTexture;
    }



    // The radius of the action (in meters). May return nothing if the action
    // doesn't have a radius specified.

    function
    actionRadius(action: rmx.Game.Action): number {
        if (action instanceof rmx.Game.SpellAction) {
            return rmx.Core.resourceContentReference<rmx.Storage.Spell>(action.spellRef).fmap(spell => {
                return spell.radius.base;
            }).get(undefined);
        }
    }



    function
    isEntityFrozen(entity: WorldObject): boolean {

        var ret = false;
        entity.auraList.forEach(x => {
            var aura = rmx.Core.resourceContent<rmx.Storage.Aura>(x.auraId).get(null);
            if (aura) {
                aura.effects.forEach(effect => {
                    if (effect.effect instanceof rmx.Storage.Petrify) {
                        ret = true;
                    }
                });
            }
        });

        return ret;
    }


    function
    animationDuration(model: rmx.Storage.Model, name: string): number {
        var animation = rmx.WebGL.findAnimationLabel(model, name);
        if (animation) {
            return (animation.end - animation.start) / animation.fps;
        } else {
            return 2;
        }
    }


    function
    entityAnimationName
    ( entity : WorldObject
    , model  : rmx.Storage.Model
    , now    : number
    , dt     : number
    ): string {

        // transition animations have priority over everything
        if(entity.animationLockUntil > now)
            return entity.transitionAnimation;

        // play correct animation if the animation deck state changes
        if(entity.prevPositionType == null) {

            entity.prevPositionType    = AnimationDeck.Ground;
            entity.animationLockUntil  = now + animationDuration(model, "spawn");
            entity.transitionAnimation = "spawn";
            return entity.transitionAnimation;

        } else if(entity.positionType != entity.prevPositionType) {

            var label = entity.animationTransitionLabel;
            entity.prevPositionType    = entity.positionType;
            entity.animationLockUntil  = now + animationDuration(model, label);
            entity.transitionAnimation = label;
            return entity.transitionAnimation;
        }

        // hack to enable animations on metaEffects (transitions missing)
        if(entity.spell) {
            var animationName = rmx.Core.resourceContent<rmx.Storage.Spell>(
                entity.spell.spellId).fmap(spell => {

                var meta = spell.sensousEffects;

                // CAST BEGIN
                if(meta.castBegin.animationId &&
                    timeWithin(entity.spell.startedAt, now, dt)) {
                    return meta.castBegin.animationId;
                }

                // CASTING
                if(meta.casting.animationId &&
                    entity.spell.startedAt < now - dt &&
                    entity.spell.startedAt + entity.spell.castTime > now) {

                    return meta.casting.animationId;
                }

                // CAST END
                if(meta.castEnd.animationId &&
                    timeWithin(entity.spell.startedAt + entity.spell.castTime, now, dt)) {
                    return meta.castEnd.animationId;
                }

            }).get(null);

            if(animationName)
                return entity.animationDeckPrefix + animationName;
        }


        // default animations
        if (entity.soul === 'fading') {
            return entity.animationDeckPrefix + 'death';
        } else if (entity.moveFlags) {
            return entity.animationDeckPrefix + 'walk';
        } else if (entity.spell) {
            return entity.animationDeckPrefix + 'attack';
        } else {
            return entity.animationDeckPrefix + 'idle';
        }
    }
}
