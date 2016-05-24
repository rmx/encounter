/// <reference path="./Ids.ts" />
/// <reference path="./Protocol.ts" />
/// <reference path="./Game.ts" />
/// <reference path="./Game/Events.ts" />
/// <reference path="./JSON.ts" />
/// <reference path="./MovePath.ts" />
/// <reference path="./Terrain.ts" />

module rmx.Pure.Message {

    // -----------------------------------------------------------------------
    export class TICK {

        constructor() {}

        static parseJSON(): TICK {
            return new TICK();
        }

        get op() {
            return rmx.Protocol.op.TICK;
        }

        get content() {
            return {};
        }
    }



    // -----------------------------------------------------------------------
    export class CONNECT {

        constructor() {}

        static parseJSON(): CONNECT {
            return new CONNECT();
        }

        get op() {
            return rmx.Protocol.op.CONNECT;
        }

        get content() {
            return {};
        }
    }





    // -----------------------------------------------------------------------
    export class SMSG_DISCONNECT {

        constructor
          ( public msg : string
          ) {}

        static parseJSON(json: any): SMSG_DISCONNECT {
            return new SMSG_DISCONNECT
                ( fromMaybe('Unknown reason', json, 'msg')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_DISCONNECT;
        }

        get content() {
            return { msg: this.msg };
        }
    }



    // -----------------------------------------------------------------------
    export class NULL {

        constructor
          ( public cookie : string
          ) {}

        static parseJSON(json: any): NULL {
            return new NULL
                ( def(json, 'cookie')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_PONG;
        }

        get content() {
            return { cookie: this.cookie };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_AUTHENTICATE {

        constructor
          ( public token : string
          , public bind  : { type: string; id: string; }
          ) {}

        static parseJSON(json: any): CMSG_AUTHENTICATE {
            return new CMSG_AUTHENTICATE
                ( def(json, 'token')
                , def(json, 'bind')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_AUTHENTICATE;
        }

        get content() {
            return { token : this.token
                   , bind  : this.bind
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_PROFILE {

        constructor
          ( public accountId : AccountId
          , public login     : string
          ) {}

        static parseJSON(json: any): SMSG_PROFILE {
            return new SMSG_PROFILE
                ( def(json, 'accountId')
                , def(json, 'login')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_PROFILE;
        }

        get content() {
            return { accountId : this.accountId
                   , login     : this.login
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_CHANGE_PARTY {

        constructor
          ( public partyId : string
          ) {}

        static parseJSON(json: any): CMSG_CHANGE_PARTY {
            return new CMSG_CHANGE_PARTY
                ( def(json, 'partyId')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_CHANGE_PARTY;
        }

        get content() {
            return { partyId: this.partyId };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_CHANGE_PARTY {

        constructor
          ( public accountId : AccountId
          , public partyId   : string
          ) {}

        static parseJSON(json: any): SMSG_CHANGE_PARTY {
            return new SMSG_CHANGE_PARTY
                ( def(json, 'accountId')
                , def(json, 'partyId')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CHANGE_PARTY;
        }

        get content() {
            return { accountId: this.accountId, partyId: this.partyId };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_CHANGE_CHARACTER {

        constructor
          ( public roleId : string
          ) {}

        static parseJSON(json: any): CMSG_CHANGE_CHARACTER {
            return new CMSG_CHANGE_CHARACTER
                ( def(json, 'roleId')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_CHANGE_CHARACTER;
        }

        get content() {
            return { roleId: this.roleId };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_CHANGE_CHARACTER {

        constructor
          ( public accountId : AccountId
          , public roleId    : string
          , public ready     : boolean
          ) {}

        static parseJSON(json: any): SMSG_CHANGE_CHARACTER {
            return new SMSG_CHANGE_CHARACTER
                ( def(json, 'accountId')
                , def(json, 'roleId')
                , def(json, 'ready')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CHANGE_CHARACTER;
        }

        get content() {
            return { accountId : this.accountId
                   , roleId    : this.roleId
                   , ready     : this.ready
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_CONFIRM_CHARACTER_CHOICE {

        constructor() {}

        static parseJSON(): CMSG_CONFIRM_CHARACTER_CHOICE {
            return new CMSG_CONFIRM_CHARACTER_CHOICE();
        }

        get op() {
            return rmx.Protocol.op.CMSG_CONFIRM_CHARACTER_CHOICE;
        }

        get content() {
            return {};
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_MOVE {

        constructor
          ( public position  : rmx.Pure.TerrainPosition
          , public moveFlags : number
          ) {}

        static parseJSON(json: any): CMSG_MOVE {
            return new CMSG_MOVE
                ( rmx.Pure.TerrainPosition.parseJSON(def(json, 'position'))
                , def(json, 'moveFlags')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_MOVE;
        }

        get content() {
            return { position  : this.position.toJSON()
                   , moveFlags : this.moveFlags
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_MOVE {

        constructor
          ( public entityId  : EntityId
          , public position  : rmx.Pure.TerrainPosition
          , public moveFlags : number
          ) {}

        static parseJSON(json: any): SMSG_MOVE {
            return new SMSG_MOVE
                ( def(json, 'entityId')
                , rmx.Pure.TerrainPosition.parseJSON(def(json, 'position'))
                , def(json, 'moveFlags')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_MOVE;
        }

        get content() {
            return { entityId  : this.entityId
                   , position  : this.position.toJSON()
                   , moveFlags : this.moveFlags
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_TELEPORT {

        constructor
          ( public entityId  : EntityId
          , public position  : rmx.Pure.TerrainPosition
          ) {}

        static parseJSON(json: any): SMSG_TELEPORT {
            return new SMSG_TELEPORT
                ( def(json, 'entityId')
                , rmx.Pure.TerrainPosition.parseJSON(def(json, 'position'))
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_TELEPORT;
        }

        get content() {
            return { entityId  : this.entityId
                   , position  : this.position.toJSON()
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_TARGET {

        constructor
          ( public id : EntityId
          ) {}

        static parseJSON(json: any): CMSG_TARGET {
            return new CMSG_TARGET
                ( def(json, 'id')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_TARGET;
        }

        get content() {
            return { id: this.id };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_SPELLCAST {

        constructor
          ( public spellId     : string
          , public spellTarget : SpellTarget
          ) {}

        static parseJSON(json: any): CMSG_SPELLCAST {
            return new CMSG_SPELLCAST
                ( def(json, 'spell')
                , SpellTarget.parseJSON(def(json, 'target'))
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_SPELLCAST;
        }

        get content() {
            return { spell  : this.spellId
                   , target : this.spellTarget.toJSON()
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_SPELLCAST_ABORT {

        constructor() {}

        static parseJSON(): CMSG_SPELLCAST_ABORT {
            return new CMSG_SPELLCAST_ABORT();
        }

        get op() {
            return rmx.Protocol.op.CMSG_SPELLCAST_ABORT;
        }

        get content() {
            return {};
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SPELLCAST_START {

        constructor
          ( public id          : EntityId
          , public casterId    : EntityId
          , public spellId     : string
          , public spellTarget : SpellTarget
          , public castTime    : number
          ) {}

        static parseJSON(json: any): SMSG_SPELLCAST_START {
            return new SMSG_SPELLCAST_START
                ( JSON.string(json, 'id')
                , JSON.string(json, 'casterId')
                , JSON.string(json, 'spellId')
                , SpellTarget.parseJSON(def(json, 'spellTarget'))
                , JSON.number(json, 'castTime')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SPELLCAST_START;
        }

        get content() {
            return { id          : this.id
                   , casterId    : this.casterId
                   , spellId     : this.spellId
                   , spellTarget : this.spellTarget.toJSON()
                   , castTime    : this.castTime
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SPELLCAST_FAILED {

        constructor
          ( public reason : string
          ) {}

        static parseJSON(json: any): SMSG_SPELLCAST_FAILED {
            return new SMSG_SPELLCAST_FAILED
                ( JSON.string(json, 'reason')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SPELLCAST_FAILED;
        }

        get content() {
            return { reason: this.reason };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SPELLCAST_FINISH {

        constructor
          ( public entityId    : EntityId
          , public targetInfos : TargetInfo[]
          ) {}

        static parseJSON(json: any): SMSG_SPELLCAST_FINISH {
            return new SMSG_SPELLCAST_FINISH
                ( JSON.string(json, 'entityId')
                , arrayFromJSON(json.targetInfos, TargetInfo.parseJSON)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SPELLCAST_FINISH;
        }

        get content() {
            return { entityId    : this.entityId
                   , targetInfos : arrayToJSON(this.targetInfos)
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SPELLCAST_CANCEL {

        constructor
          ( public entityId : EntityId
          ) {}

        static parseJSON(json: any): SMSG_SPELLCAST_CANCEL {
            return new SMSG_SPELLCAST_CANCEL
                ( JSON.string(json, 'entityId')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SPELLCAST_CANCEL;
        }

        get content() {
            return { entityId: this.entityId };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_JOIN {

        constructor
          ( public accountId          : AccountId
          , public partyId            : string
          , public roleId             : string
          , public ready              : boolean
          , public controlledEntityId : EntityId
          , public isConnected        : boolean
          ) {}

        static parseJSON(json: any): SMSG_JOIN {
            return new SMSG_JOIN
                ( def(json, 'id')
                , def(json, 'partyId')
                , def(json, 'roleId')
                , def(json, 'ready')
                , def(json, 'controlledEntityId')
                , def(json, 'isConnected')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_JOIN;
        }

        get content() {
            return { id                 : this.accountId
                   , partyId            : this.partyId
                   , roleId             : this.roleId
                   , ready              : this.ready
                   , controlledEntityId : this.controlledEntityId
                   , isConnected        : this.isConnected
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_LEAVE {

        constructor
          ( public playerId : AccountId
          ) {}

        static parseJSON(json: any): SMSG_LEAVE {
            return new SMSG_LEAVE
                ( def(json, 'id')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_LEAVE;
        }

        get content() {
            return { id: this.playerId };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_DESTROY_ENTITY {

        constructor
          ( public entityId : EntityId
          ) {}

        static parseJSON(json: any): SMSG_DESTROY_ENTITY {
            return new SMSG_DESTROY_ENTITY
                ( def(json, 'id')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_ENTITY_REMOVE;
        }

        get content() {
            return { id : this.entityId };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_PONG {

        constructor
          ( public cookie : string
          ) {}

        static parseJSON(json: any): SMSG_PONG {
            return new SMSG_PONG
                ( def(json, 'cookie')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_PONG;
        }

        get content() {
            return { cookie : this.cookie };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_CHATMESSAGE {

        constructor
          ( public text : string
          ) {}

        static parseJSON(json: any): CMSG_CHATMESSAGE {
            return new CMSG_CHATMESSAGE
                ( def(json, 'text')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_CHATMESSAGE;
        }

        get content() {
            return { text : this.text };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_CHATMESSAGE {

        constructor
          ( public senderId : AccountId
          , public text     : string
          ) {}

        static parseJSON(json: any): SMSG_CHATMESSAGE {
            return new SMSG_CHATMESSAGE
                ( def(json, 'senderId')
                , def(json, 'text')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CHATMESSAGE;
        }

        get content() {
            return { senderId : this.senderId
                   , text     : this.text
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SCORE {

        constructor
          ( public partyId : string
          , public score   : number
          ) {}

        static parseJSON(json: any): SMSG_SCORE {
            return new SMSG_SCORE
                ( def(json, 'partyId')
                , def(json, 'score')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SCORE;
        }

        get content() {
            return { partyId : this.partyId
                   , score   : this.score
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_ENTITY_UPDATE {

        constructor
          ( public entityId : EntityId
          , public fields   : any
          ) {}

        static parseJSON(json: any): SMSG_ENTITY_UPDATE {
            return new SMSG_ENTITY_UPDATE
                ( def(json, 'id')
                , def(json, 'fields')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_ENTITY_UPDATE;
        }

        get content() {
            return { id     : this.entityId
                   , fields : this.fields
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_ENTITY_POWER {

        constructor
          ( public entityId  : EntityId
          , public powerType : string
          , public value     : number
          ) {}

        static parseJSON(json: any): SMSG_ENTITY_POWER {
            return new SMSG_ENTITY_POWER
                ( def(json, 'id')
                , def(json, 'type')
                , def(json, 'value')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_ENTITY_POWER;
        }

        get content() {
            return { id    : this.entityId
                   , type  : this.powerType
                   , value : this.value
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_UPDATE_AURA {

        constructor
          ( public aura : Aura
          ) {}

        static parseJSON(json: any): SMSG_UPDATE_AURA {
            return new SMSG_UPDATE_AURA(Aura.parseJSON(json));
        }

        get op() {
            return rmx.Protocol.op.SMSG_UPDATE_AURA;
        }

        get content() {
            return this.aura.toJSON();
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_REMOVE_AURA {

        constructor
          ( public holderId : EntityId
          , public slot     : string
          ) {}

        static parseJSON(json: any): SMSG_REMOVE_AURA {
            return new SMSG_REMOVE_AURA
                ( JSON.string(json, 'holderId')
                , JSON.string(json, 'slot')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_REMOVE_AURA;
        }

        get content() {
            return { holderId : this.holderId
                   , slot     : this.slot
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_SPELL_COOLDOWNS {

        constructor
          ( public entityId : EntityId
          , public spells   : SpellCooldownEntry[]
          ) {}

        static parseJSON(json: any): SMSG_SPELL_COOLDOWNS {
            return new SMSG_SPELL_COOLDOWNS
                ( def(json, 'id')
                , arrayFromJSON(def(json, 'spells'), SpellCooldownEntry.parseJSON)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SPELL_COOLDOWNS;
        }

        get content() {
            return { id     : this.entityId
                   , spells : arrayToJSON(this.spells)
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_CONTROL {

        constructor
          ( public accountId : AccountId
          , public entityId  : EntityId
          ) {}

        static parseJSON(json: any): SMSG_CONTROL {
            return new SMSG_CONTROL
                ( JSON.string(json, 'accountID')
                , JSON.string(json, 'controlledEntityId')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CONTROL;
        }

        get content() {
            return { accountID          : this.accountId
                   , controlledEntityId : this.entityId
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class SMSG_INITIAL_SPELLS {

        constructor
          ( public entityId : EntityId
          , public spells   : SpellEntry[]
          ) {}

        static parseJSON(json: any): SMSG_INITIAL_SPELLS {
            return new SMSG_INITIAL_SPELLS
                ( JSON.string(json, 'entityId')
                , arrayFromJSON(def(json, 'spells'), SpellEntry.parseJSON)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_INITIAL_SPELLS;
        }

        get content() {
            return { entityId : this.entityId
                   , spells   : arrayToJSON(this.spells)
                   };
        }
    }



    // -----------------------------------------------------------------------
    export class CMSG_INVITE {

        constructor
          ( public accountId : AccountId
          ) {}

        static parseJSON(json: any): CMSG_INVITE {
            return new CMSG_INVITE
                ( JSON.string(json, 'accountId')
                );
        }

        get op() {
            return rmx.Protocol.op.CMSG_INVITE;
        }

        get content() {
            return { accountId: this.accountId };
        }
    }



    // SMSG_METADATA
    // -----------------------------------------------------------------------
    //
    // Static metadata about the game. Things which don't change during the
    // course of a game, such as the encounterId.

    export class SMSG_METADATA {

        constructor
          ( public encounterId : string
          ) {}

        static parseJSON(json: any): SMSG_METADATA {
            return new SMSG_METADATA
                ( JSON.string(json, 'encounterId')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_METADATA;
        }

        get content() {
            return { encounterId: this.encounterId };
        }
    }



    // SMSG_UPDATE_OBJECTIVE
    // -----------------------------------------------------------------------

    export class SMSG_UPDATE_OBJECTIVE {

        constructor
          ( public partyId     : string
          , public objectiveId : string
          , public isCompleted : boolean
          ) {}

        static parseJSON(json: any): SMSG_UPDATE_OBJECTIVE {
            return new SMSG_UPDATE_OBJECTIVE
                ( JSON.string(json, 'partyId')
                , JSON.string(json, 'objectiveId')
                , JSON.boolean(json, 'isCompleted')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_UPDATE_OBJECTIVE;
        }

        get content() {
            return { partyId     : this.partyId
                   , objectiveId : this.objectiveId
                   , isCompleted : this.isCompleted
                   };
        }
    }



    // SMSG_CHANGE_STAGE
    // -----------------------------------------------------------------------
    //
    // Sent to the client when the game stage changes. The only possible
    // transitions are Setup -> Playing -> Finished.

    export class SMSG_CHANGE_STAGE {

        constructor
          ( public stage : Stage
          ) {}

        static parseJSON(json: any): SMSG_CHANGE_STAGE {
            return new SMSG_CHANGE_STAGE
                ( JSON.number(json, 'stage')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CHANGE_STAGE;
        }

        get content() {
            return { stage: this.stage };
        }
    }



    // SMSG_CREATE_PROJECTILE
    // -----------------------------------------------------------------------

    export class SMSG_CREATE_PROJECTILE {

        constructor
          ( public projectile : Projectile
          ) {}

        static parseJSON(json: any): SMSG_CREATE_PROJECTILE {
            return new SMSG_CREATE_PROJECTILE
                ( Projectile.parseJSON(json)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CREATE_PROJECTILE;
        }

        get content() {
            return this.projectile.toJSON();
        }
    }



    // SMSG_CREATE_TERRAIN
    // -----------------------------------------------------------------------

    export class SMSG_CREATE_TERRAIN {

        constructor
          ( public terrain : rmx.Pure.Terrain
          ) {}

        static parseJSON(json: any): SMSG_CREATE_TERRAIN {
            return new SMSG_CREATE_TERRAIN
                ( rmx.Pure.Terrain.parseJSON(json)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CREATE_TERRAIN;
        }

        get content() {
            return this.terrain.toJSON();
        }
    }



    // SMSG_CREATE_GROUNDAREAEFFECT
    // -----------------------------------------------------------------------

    export class SMSG_CREATE_GROUNDAREAEFFECT {

        constructor
          ( public gae : rmx.Pure.GroundAreaEffect
          ) {}

        static parseJSON(json: any): SMSG_CREATE_GROUNDAREAEFFECT {
            return new SMSG_CREATE_GROUNDAREAEFFECT
                ( rmx.Pure.GroundAreaEffect.parseJSON(json)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_CREATE_GROUNDAREAEFFECT;
        }

        get content() {
            return this.gae.toJSON();
        }
    }



    // SMSG_SAY
    // -----------------------------------------------------------------------

    export class SMSG_SAY {

        constructor
          ( public entityId : EntityId
          , public message  : string
          ) {}

        static parseJSON(json: any): SMSG_SAY {
            return new SMSG_SAY
                ( JSON.string(json, 'entityId')
                , JSON.string(json, 'message')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_SAY;
        }

        get content() {
            return { entityId: this.entityId, message: this.message };
        }
    }



    // SMSG_DEBUG
    // -----------------------------------------------------------------------

    export class SMSG_DEBUG {

        constructor
          ( public text : string
          ) {}

        static parseJSON(json: any): SMSG_DEBUG {
            return new SMSG_DEBUG
                ( JSON.string(json, 'text')
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_DEBUG;
        }

        get content() {
            return { text: this.text };
        }
    }



    // SMSG_COMBAT_EVENT
    // -----------------------------------------------------------------------

    export class SMSG_COMBAT_EVENT {

        constructor
          ( public event : rmx.Pure.Game.CombatEvent
          ) {}

        static parseJSON(json): SMSG_COMBAT_EVENT {
            return new SMSG_COMBAT_EVENT
                ( rmx.Pure.Game.CombatEvent.parseJSON(json)
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_COMBAT_EVENT;
        }

        get content() {
            return this.event.toJSON();
        }
    }



    // SMSG_MOVEPATH
    // -----------------------------------------------------------------------

    export class SMSG_MOVEPATH {

        constructor
          ( public entityId : EntityId
          , public movePath : rmx.Pure.MovePath // Maybe
          ) {}

        static parseJSON(json): SMSG_MOVEPATH {
            return new SMSG_MOVEPATH
                ( def(json, 'id')
                , opt(json, 'path', () => {
                    return new rmx.Pure.MovePath(json.path, json.heading);
                  })
                );
        }

        get op() {
            return rmx.Protocol.op.SMSG_MOVEPATH;
        }

        get content() {
            return { id      : this.entityId
                   , path    : this.movePath ? this.movePath.path : null
                   , heading : this.movePath ? this.movePath.heading : null
                   };
        }
    }
}
