/// <reference path="../../ext/gl-matrix.ts" />
/// <reference path="../../ext/underscore.ts" />

/// <reference path="./Math.ts" />
/// <reference path="./Task.ts" />
/// <reference path="./Terrain.ts" />
/// <reference path="./String.ts" />
/// <reference path="./JSON.ts" />

/// <reference path="./Game/Elevation.ts" />

// This file contains pure types used in the game. These types must be
// portable to the client.

module rmx.Pure {


    // IEntity
    // -----------------------------------------------------------------------

    export interface IEntity {
        id : EntityId;
    }



    // IState
    // -----------------------------------------------------------------------

    export interface IState {

        gameId : string;
        // ^ The Id of the game. This is always known, initialized when the
        // state is created.

        encounterId : string;
        // ^ The Id of the encounter from which the game was created. On the
        // client this is initialized asynchronously.

        stage : Stage;
        // ^ The stage in which the game is. The client uses 'Setup' until
        // gets the true state from the server.

        parties : Party[];
        // ^ All the parties which participate in the game. Some may be empty!

        entities : Map<EntityId, IEntity>;
        // ^ All the entities in the game.
    }


    // isDeserted
    // -----------------------------------------------------------------------
    //
    // True if all players are disconnected. This state may be only temporary
    // if players are reloading their clients. Or permanent at the end of the
    // game when all players have moved on.

    export function
    isDeserted(state: IState): boolean {
        return state.parties.reduce((x: boolean, party) => {
            return party.players.reduce((y, player) => {
                return y && !player.isConnected;
            }, x);
        }, true);
    }


    // registerEntity
    // -----------------------------------------------------------------------

    export function
    registerEntity(state: IState, entity: IEntity): void {
        state.entities.set(entity.id, entity);
    }


    // lookupEntity
    // -----------------------------------------------------------------------

    export function
    lookupEntity<T extends IEntity>(state: IState, entityId: EntityId): T {
        return <T> state.entities.get(entityId);
    }


    // destroyEntity
    // -----------------------------------------------------------------------

    export function
    destroyEntity(state: IState, entityId: EntityId): void {
        state.entities.delete(entityId);
    }


    // withEntity
    // -----------------------------------------------------------------------
    //
    // This is basically a lookupEntity, but it handles non-existing entities
    // gracefully and checks the type (a wrong type is a hard failure).

    export function
    withEntity<T extends IEntity>
    ( state    : IState
    , entityId : EntityId
    , ctor     : new(...args: any[]) => T
    , fn       : (entity: T) => void
    ): void {
        var entity = lookupEntity(state, entityId);
        if (entity) {
            if (entity instanceof ctor) {
                fn(<T>entity);
            } else {
                throw new Error(
                    [ "withEntity: Unexpected type ("
                    , (<any>entity.constructor).name
                    , ")"
                    ].join('')
                );
            }
        }
    }

    // lookupParty
    // -----------------------------------------------------------------------

    export function
    lookupParty(state: IState, partyId: string): Party {
        for (var i = 0; i < state.parties.length; ++i) {
            if (state.parties[i].id === partyId) {
                return state.parties[i];
            }
        }
    }


    // lookupPlayer
    // -----------------------------------------------------------------------

    export function
    lookupPlayer(state: IState, accountId: AccountId): Player {
        for (var i = 0; i < state.parties.length; ++i) {
            var party  = state.parties[i]
              , player = party.get(accountId);

            if (player) {
                return player;
            }
        }
    }


    // playerParty
    // -----------------------------------------------------------------------
    //
    // The party in which the player is. This is always defined, because
    // a player always needs to be in one of the parties.

    export function
    playerParty(state: IState, accountId: AccountId): Party {
        for (var i = 0; i < state.parties.length; ++i) {
            var party  = state.parties[i]
              , player = party.get(accountId);

            if (player) {
                return party;
            }
        }
    }


    // entityParty
    // -----------------------------------------------------------------------
    //
    // If the entity is controlled by a player, this return the players'
    // party.

    export function
    entityParty(state: IState, entity: IEntity): Party {
        var player = controllingPlayer(state, entity);
        if (player) {
            return playerParty(state, player.accountId);
        }
    }


    // controllingPlayer
    // -----------------------------------------------------------------------
    //
    // Return the 'Player' who is controlling the given entity. The function
    // returns empty if no player is currently controlling the entity.

    export function
    controllingPlayer(state: IState, entity: IEntity): Player {
        for (var i = 0; i < state.parties.length; ++i) {
            var party  = state.parties[i]
              , player = party.playerControllingEntity(entity.id);

            if (player) {
                return player;
            }
        }
    }



    // playerControlledEntityId
    // -----------------------------------------------------------------------

    export function
    playerControlledEntityId(state: IState, accountId: AccountId): EntityId {
        var player = lookupPlayer(state, accountId);
        if (player) {
            return player.entityId;
        }
    }



    // playerControlledEntity
    // -----------------------------------------------------------------------

    export function
    playerControlledEntity<T extends IEntity>
    (state: IState, accountId: AccountId): T {
        var entityId = playerControlledEntityId(state, accountId);
        if (entityId) {
            return lookupEntity<T>(state, entityId);
        }
    }



    // Player
    // -----------------------------------------------------------------------

    export class Player {

        constructor
          ( public accountId : AccountId
            // ^ The player's AccountId.

          , public roleId    : string
            // ^ Read-write during Setup stage, read-only while the game
            // is running. It is used to instanciate the Entity when all
            // players are ready and the game begins.
            //
            // See CMSG_CHANGE_CHARACTER.

          , public entityId  : EntityId
            // ^ Null during Setup phase. Set once we create the entity for
            // this player. In theory can change during the game.
            //
            // See SMSG_CONTROL.

          , public ready     : boolean
            // ^ Only used during the Setup stage. Only when all players are
            // ready we can transition from Setup to Running.
            //
            // See CMSG_CONFIRM_CHARACTER_CHOICE.

          , public isConnected : boolean
            // ^ True if the player is connected to the shard. This is
            // currently only used on the server to detect when the game is
            // deserted.
            //
            // Later we'll also forward this field to the clients so they can
            // see who is connected and who not.
            //
            // The game could also use this to avoid attacking disconnected
            // players, or even temporarily remove the controlled entity.

          ) {}
    }



    // Party
    // -----------------------------------------------------------------------

    export class Party {

        constructor
          ( public id         : string
          , public score      : number = 0
          , public players    : Player[] = []
          , public objectives : Objective[] = []
          ) {}

        get allAccountIds(): AccountId[] {
            return this.players.map(player => {
                return player.accountId;
            });
        }

        remove(accountId: AccountId): void {
            this.players = this.players.filter(function(x) {
                return x.accountId !== accountId;
            });
        }

        get(accountId: AccountId): Player {
            return this.players.filter(function(x) {
                return x.accountId === accountId;
            })[0];
        }

        playerControllingEntity(id: EntityId): Player {
            return _.find(this.players, function(x: Player) {
                return x.entityId === id;
            });
        }
    }


    // objectivesCompleted
    // -----------------------------------------------------------------------

    export function
    objectivesCompleted(party: rmx.Pure.Party): boolean {
        return party.objectives.every(objective => {
            return objective.isCompleted;
        });
    }



    // numCompletedObjectives
    // -----------------------------------------------------------------------

    export function
    numCompletedObjectives(party: rmx.Pure.Party): number {
        return party.objectives.filter(objective => {
            return objective.isCompleted;
        }).length;
    }



    // Objective
    // -----------------------------------------------------------------------
    //
    // At runtime we cache certain properties of objectives. For example to
    // ensure that objectives which were at one point completed stay so.

    export class Objective {

        public isCompleted : boolean = false;

        constructor
          ( public id    : string
          , public tasks : rmx.Pure.Task.Task<any>[]
          ) {}
    }



    // Stage
    // -----------------------------------------------------------------------

    export enum Stage { Blank, Setup, Running, Finished };



    // CooldownTimer
    // -----------------------------------------------------------------------

    export class CooldownTimer {

        constructor
          ( public start    : number
          , public duration : number
          ) {}

        isActive(currentTime: number): boolean {
            return this.start + this.duration >= currentTime;
        }
    }



    // Power
    // -----------------------------------------------------------------------

    export class Power {

        constructor
          ( public value    : number
          , public maxValue : number
          ) {}

        modify(diff: number): number {
            return this.value = clamp(0, this.maxValue, this.value + diff);
        }
    }



    // Soul
    // -----------------------------------------------------------------------
    //
    // WorldObjects have a soul.

    export enum Soul { Present, Fading };



    // SpellTarget
    // -----------------------------------------------------------------------

    export enum SpellTargetType { WorldObject, Location };

    export class SpellTarget {

        constructor
          ( public entityId : EntityId
          , public location : number[]
          ) {}

        get type(): SpellTargetType {
            if (this.entityId != null) {
                return SpellTargetType.WorldObject;
            } else if (this.location != null) {
                return SpellTargetType.Location;
            } else {
                throw new Error('Unknown SpellTarget type');
            }
        }

        static parseJSON(json): SpellTarget {
            var type = <any> SpellTargetType[json.type];

            switch (type) {
            case SpellTargetType.WorldObject:
                return new SpellTarget(json.id, null);

            case SpellTargetType.Location:
                return new SpellTarget(null, json.location);

            default:
                throw new Error("Could not decode SpellTarget: " + json);
            }
        }

        toJSON(): any {
            return { type     : SpellTargetType[this.type]
                   , id       : this.entityId
                   , location : this.location
                   };
        }
    }



    // SpellEntry
    // -----------------------------------------------------------------------
    //
    // Describes a spell in a WorldObject's spellbook. This is used in the
    // SMSG_INITIAL_SPELLS message.

    export class SpellEntry {

        constructor
          ( public id : string
          ) {}

        static parseJSON(json): SpellEntry {
            return new SpellEntry
                ( JSON.string(json, 'id')
                );
        }

        toJSON() {
            return { id : this.id
                   };
        }
    }



    // Aura
    // -----------------------------------------------------------------------

    export class Aura {

        constructor
          ( public casterId     : rmx.Pure.EntityId
            // ^ The original caster who added the aura to the holder.

          , public holderId     : rmx.Pure.EntityId
            // ^ The entity to which this aura is attached to.

          , public slot         : string
            // ^ A unique slot on the entity where this aura is stored.

          , public auraId       : string
            // ^ The AuraId.

          , public createdAt    : number
            // ^ The game time when the aura was originally added to the
            // holder.

          , public energy       : number
            // ^ The amount of energy the aura has. When this drops to zero
            // the aura is removed.

          , public stackCount   : number
            // ^ The number of stacks. Can change during the lifetime of the
            // aura.

          , public duration     : number
            // How long the aura remains on the target, starting at the
            // 'createdAt' time. Can change during the lifetime of the aura.

          , public tickInterval : number
            // ^ The interval between ticks.
          ) {}

        toString() {
            return toString(this, 'Aura', 'casterId', 'holderId', 'slot', 'auraId');
        }

        static parseJSON(json): Aura {
            return new Aura
                ( JSON.string(json, 'casterId')
                , JSON.string(json, 'holderId')
                , JSON.string(json, 'slot')
                , JSON.string(json, 'auraId')
                , JSON.number(json, 'createdAt')
                , opt(json, 'energy',       JSON.number)
                , opt(json, 'stackCount',   JSON.number)
                , opt(json, 'duration',     JSON.number)
                , opt(json, 'tickInterval', JSON.number)
                );
        }

        toJSON() {
            return { casterId     : this.casterId
                   , holderId     : this.holderId
                   , slot         : this.slot
                   , auraId       : this.auraId
                   , createdAt    : this.createdAt
                   , energy       : this.energy
                   , stackCount   : this.stackCount
                   , duration     : this.duration
                   , tickInterval : this.tickInterval
                   };
        }
    }



    // Reaction
    // -----------------------------------------------------------------------

    export enum Reaction { Friendly, Hostile };



    // GroundAreaEffect
    // -----------------------------------------------------------------------

    export class GroundAreaEffect {

        position : Vec3;
        // ^ The computed position from the terrainPosition.

        constructor
          ( public id              : EntityId
          , public casterId        : EntityId
          , public createdAt       : number
          , public terrainPosition : TerrainPosition
          , public duration        : number
          , public radius          : number
          , public auraId          : string
          ) {}

        static parseJSON(json: any): GroundAreaEffect {
            return new GroundAreaEffect
                ( JSON.string(json, 'id')
                , JSON.string(json, 'casterId')
                , JSON.number(json, 'createdAt')
                , TerrainPosition.parseJSON(json.terrainPosition)
                , JSON.number(json, 'duration')
                , JSON.number(json, 'radius')
                , JSON.string(json, 'auraId')
                );
        }

        toJSON() {
            return { id              : this.id
                   , casterId        : this.casterId
                   , createdAt       : this.createdAt
                   , terrainPosition : this.terrainPosition.toJSON()
                   , duration        : this.duration
                   , radius          : this.radius
                   , auraId          : this.auraId
                   };
        }

        toString() {
            return toString(this, 'GroundAreaEffect', 'id');
        }
    }



    // Spell
    // -----------------------------------------------------------------------
    //
    // The toJSON / parseJSON functions aren't defined here. The relevant code
    // is in rmx.Pure.Messages (SMSG_SPELLCAST_START).

    export class Spell {

        constructor
          ( public id          : EntityId
            // ^ A unique id identifying the spell. It is used on the client
            // to create VFX related to the spell.

          , public casterId    : EntityId
            // ^ The entity which is casting this spell.

          , public spellId     : string
            // ^ The spell.

          , public spellTarget : SpellTarget
            // ^ Spell target as supplied by the client or behavior script.

          , public startedAt   : number
            // ^ The time (local) when the spell was begun casting.

          , public castTime    : number
            // ^ The cast time computed at the moment when the spellcast was
            // started.

          , public pulseTime   : number
            // ^ The pulse time computed at the moment when the spellcast was
            // started. May be undefined.

          , public targetInfos : TargetInfo[]
            // ^ Once the spell is finished casting, this will be filled with
            // the resolved targets. Until then, this is null.
          ) {}

        toString() {
            return toString(this, 'Spell', 'id', 'casterId', 'spellId');
        }

        static parseJSON(json): Spell {
            return new Spell
                ( JSON.string(json, 'id')
                , JSON.string(json, 'casterId')
                , JSON.string(json, 'spellId')
                , SpellTarget.parseJSON(json.spellTarget)
                , JSON.number(json, 'startedAt')
                , JSON.number(json, 'castTime')
                , opt(json, 'pulseTime', JSON.number)
                , arrayFromJSON(json.targetInfos, TargetInfo.parseJSON)
                );
        }

        toJSON() {
            return { id          : this.id
                   , casterId    : this.casterId
                   , spellId     : this.spellId
                   , spellTarget : this.spellTarget.toJSON()
                   , startedAt   : this.startedAt
                   , castTime    : this.castTime
                   , pulseTime   : this.pulseTime
                   , targetInfos : arrayToJSON(this.targetInfos)
                   };
        }

    }



    // TargetInfo
    // -----------------------------------------------------------------------

    export class TargetInfo {

        constructor
          ( public spellTarget : SpellTarget
          , public effectMask  : number
          ) {
        }

        static parseJSON(json): TargetInfo {
            return new TargetInfo
                ( SpellTarget.parseJSON(json.spellTarget)
                , JSON.number(json, 'effectMask')
                );
        }

        toJSON() {
            return { spellTarget : this.spellTarget.toJSON()
                   , effectMask  : this.effectMask
                   };
        }
    }



    // Projectile
    // -----------------------------------------------------------------------

    export class Projectile {

        constructor
          ( public id         : EntityId
            // ^ A projectile is an entity, so it gets an EntityId.

          , public spell      : Spell
            // ^ The spell on whose behalf this projectile was created. It is
            // needed so that the projectile knows which spell effects need to
            // be applied when the projectile impacts. Also to know who the
            // caster was.

          , public position   : Vec3
            // ^ The current position of the projectile. This field is
            // currently only updated on the client. The server ignores this.

          , public targetInfo : TargetInfo
            // ^ This is the result of the target resolver chain. It includes
            // the spell target towards which the projectile is flying, as
            // well as a bitmap of effects which should be applied upon
            // impact.

          , public createdAt  : number
            // ^ Time when the projectile was created.

          , public delay      : number
            // ^ Time from its creation time when the projectile reaches its
            // target.
          ) {
        }

        toString() {
            return toString(this, 'Projectile', 'id');
        }

        static parseJSON(json): Projectile {
            return new Projectile
                ( JSON.string(json, 'id')
                , Spell.parseJSON(json.spell)
                , vec3.clone(json.position)
                , TargetInfo.parseJSON(json.targetInfo)
                , JSON.number(json, 'createdAt')
                , JSON.number(json, 'delay')
                );
        }

        toJSON() {
            var p = this.position;

            return { id         : this.id
                   , spell      : this.spell.toJSON()
                   , position   : [p[0],p[1],p[2]]
                   , targetInfo : this.targetInfo.toJSON()
                   , createdAt  : this.createdAt
                   , delay      : this.delay
                   };
        }
    }



    // Attribute
    // -----------------------------------------------------------------------

    export class Attribute {

        value : number;

        constructor
          ( public formula   : Formula
          , public base      : number
          , public sanitizer : (x: number) => number = (x) => { return x; }
          , public modifiers : FormulaModifier[] = []
          ) {
            updateAttribute(this, modifiers);
        }
    }


    export function
    updateAttribute(attr: Attribute, modifiers: FormulaModifier[]): void {
        attr.value = attr.sanitizer(attr.formula.apply(attr.base, modifiers));
    }



    // Combinator
    // -----------------------------------------------------------------------

    export class Combinator {
        static sum     = (x) => { return x.reduce(((a, i) => { return a + i; }), 0); };
        static product = (x) => { return x.reduce(((a, i) => { return a * i; }), 1); };

        constructor
          ( public cA : (xs: number[]) => number
          , public cB : (xs: number[]) => number
          , public cC : (xs: number[]) => number
          , public cD : (xs: number[]) => number
          ) {}
    }




    // FormulaModifier
    // -----------------------------------------------------------------------

    export class FormulaModifier {
        constructor
          ( public pA : number
          , public pB : number
          , public pC : number
          , public pD : number
          ) {
        }
    }



    // Bounds
    // -----------------------------------------------------------------------

    export class Bounds {

        constructor
          ( public min : number
          , public max : number
          ) {}

        clamp(x: number): number {
            return clamp(this.min, this.max, x);
        }
    }



    // Formula
    // -----------------------------------------------------------------------

    function isDefined(x): boolean {
        return x != null;
    }

    export class Formula {

        constructor
          ( public combinator : Combinator
          , public evaluator  : Function
          , public bounds     : Bounds
          ) {}

        apply(base: number, mod: FormulaModifier[]): number {
            var cmb = this.combinator
              , a   = cmb.cA(mod.map((x) => { return x.pA; }).filter(isDefined))
              , b   = cmb.cB(mod.map((x) => { return x.pB; }).filter(isDefined))
              , c   = cmb.cC(mod.map((x) => { return x.pC; }).filter(isDefined))
              , d   = cmb.cD(mod.map((x) => { return x.pD; }).filter(isDefined));

            return this.bounds.clamp(this.evaluator(base, a, b, c, d));
        }
    }



    // ThreatManager
    // -----------------------------------------------------------------------

    export class ThreatManager {

        constructor
          ( public threatList : ThreatReference[]
          ) {}

        // FIXME: The entity in the returned ThreatReference may not exist.
        // Use with caution.
        get topThreatReference(): ThreatReference {
            return this.threatList[0];
        }

        private findReference(entityId: EntityId): ThreatReference {
            return _.find(this.threatList, function(x) {
                return x.entityId === entityId;
            });
        }

        hasThreatReference(entityId: EntityId): boolean {
            return !! this.findReference(entityId);
        }

        getThreat(entityId: EntityId): number {
            var ref = this.findReference(entityId);
            return ref ? ref.threat : null;
        }

        addThreat(entityId: EntityId, threat: number): void {
            var ref           = this.findReference(entityId)
              , oldThreatList = this.threatList;

            if (ref) {
                ref.modifyThreat(threat);
            } else {
                this.threatList.push(new ThreatReference(entityId, threat));
                oldThreatList = [];
            }

            this.sortThreatList(oldThreatList);
        }

        setTopThreat(entityId: EntityId, factor: number): void {
            var ref           = this.findReference(entityId)
              , oldThreatList = this.threatList
              , topThreatRef  = this.topThreatReference
              , topThreat     = topThreatRef ? topThreatRef.threat : 0
              , newThreat     = factor * topThreat;

            if (ref) {
                ref.threat = newThreat;
            } else {
                this.threatList.push(new ThreatReference(entityId, newThreat));
                oldThreatList = [];
            }

            this.sortThreatList(oldThreatList);
        }

        removeFromThreatList(entityId: EntityId): void {
            var oldThreatList = this.threatList;
            this.threatList = _.filter(oldThreatList, function(x) {
                return x.entityId !== entityId;
            });

            this.sortThreatList(oldThreatList);
        }

        getThreatList(): EntityId[] {
            return this.threatList.map(function(x) {
                return x.entityId;
            });
        }

        private sortThreatList(oldThreatList: ThreatReference[]): void {
            this.threatList = _.sortBy(this.threatList, function(ref) {
                return -ref.threat;
            });

            // FIXME: unused variable warning.
            (() => { return oldThreatList; })();

            // if (arraysNotEqual(oldThreatList, this.threatList)) {
                // TODO: Use a typed event: Event<ThreatChanged>.
                // this.owner.emitEvent("ThreatChanged", {});
            // }
        }
    }



    // ThreatReference
    // -----------------------------------------------------------------------
    //
    // A ThreatReference tracks how much threat somebody has against the
    // entity. This is an internal class used by the ThreatManager.

    export class ThreatReference {
        constructor
          ( public entityId : EntityId
          , public threat   : number
            // ^ A positive number.
          ) {}

        modifyThreat(diff: number): void {
            this.threat = clamp(0, Number.MAX_VALUE, this.threat + diff);
        }
    }



    // SpellCooldownEntry
    // -----------------------------------------------------------------------
    //
    // This is broadcast by the server to client to indicate which spells
    // are on cooldown.

    export class SpellCooldownEntry {

        constructor
          ( public spellId       : string
          , public start         : number
          , public duration      : number
          ) {}

        static parseJSON(json) {
            var cooldownTimer = JSON.object(json, 'cooldownTimer');

            return new SpellCooldownEntry
                ( JSON.string(json, 'id')
                , JSON.number(cooldownTimer, 'start')
                , JSON.number(cooldownTimer, 'duration')
                );
        }

        toJSON() {
            // XXX: This has the same structure as 'CooldownTimer'.
            var cooldownTimer =
                { start    : this.start
                , duration : this.duration
                };

            return { id            : this.spellId
                   , cooldownTimer : cooldownTimer
                   };
        }
    }
}
