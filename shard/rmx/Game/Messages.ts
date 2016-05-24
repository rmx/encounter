
import * as Protocol from '../Pure/Protocol';
import * as M from '../Pure/Message';
import { WorldObject, spellCooldownTimer } from '../Game/Entities/WorldObject';
import { GroundAreaEffect } from '../Game/Entities/GroundAreaEffect';
import { Party, Player, SpellCooldownEntry, Soul, SpellEntry } from '../Pure/Game';
import { Elevation } from '../Pure/Game/Elevation';
import * as J from '../Pure/JSON';
import * as PG from '../Pure/Game';



// Messages are sent to the client so it can update its local game state.
export interface Message {
    op      : number;
    content : any;
}



export function
SMSG_FIELDS(entity: WorldObject, fields): Message {
    var content =
        { id     : entity.id
        , fields : fields
        };

    return { op: Protocol.op.SMSG_ENTITY_UPDATE, content: content };
}


export function
SMSG_INITIAL_SPELLS(entity: WorldObject): Message {
    var spells = entity.spells.map(spellId => {
        return new SpellEntry(spellId);
    });

    return new M.SMSG_INITIAL_SPELLS(entity.id, spells);
}


export function
SMSG_CREATE_WORLDOBJECT(entity: WorldObject): Message {
    var powers = {}, name;
    for (name in entity.powers) {
        var power = entity.powers[name];
        powers[name] = { value    : power.value
                       , maxValue : power.maxValue
                       };
    }

    var auras = [];
    for (var slot in entity.auras) {
        auras.push(entity.auras[slot].toJSON());
    }

    var fields =
        { activePowerType     : entity.activePower
        , appearance          : entity.appearance
        , faction             : entity.faction
        , health              : entity.health
        , maxHealth           : entity.maxHealth.value
        , moveFlags           : entity.moveFlags
        , movementSpeed       : entity.movementSpeed.value
        , name                : entity.name
        , phase               : entity.phase
        , positionType        : entity.positionType
        , scale               : entity.scale.value
        , soul                : J.enumToJSON(Soul, entity.soul)
        , targetId            : entity.targetId
        , elevation           : J.enumToJSON(Elevation, entity.elevation)
        , elevationTransition : entity.elevationTransition.toJSON()
        };


    var content =
         { id       : entity.id
         , position : entity.terrainPosition.toJSON()
         , auras    : auras
         , fields   : fields
         , powers   : powers
         };

    return { op: Protocol.op.SMSG_CREATE_WORLDOBJECT, content: content };
}


export function
SMSG_CREATE_GROUNDAREAEFFECT(gae0: GroundAreaEffect): Message {
    var gae = new PG.GroundAreaEffect
        ( gae0.id
        , gae0.casterId
        , gae0.createdAt
        , gae0.terrainPosition
        , gae0.duration
        , gae0.radius
        , gae0.auraId
        );

    return new M.SMSG_CREATE_GROUNDAREAEFFECT(gae);
}


export function
SMSG_CONTROL(player: Player): Message {
    return new M.SMSG_CONTROL
        ( player.accountId
        , player.entityId
        );
}


export function
SMSG_SPELL_COOLDOWNS(entity: WorldObject): Message {
    var spells = entity.spells.map(spellId => {
        var timer    = spellCooldownTimer(entity, spellId)
          , start    = timer.start
          , duration = timer.duration;

        return new SpellCooldownEntry(spellId, start, duration);
    });

    return new M.SMSG_SPELL_COOLDOWNS(entity.id, spells);
}


export function
SMSG_MOVEPATH(entity: WorldObject): Message {
    var content =
        { id      : entity.id
        , path    : entity.movePath ? entity.movePath.path : null
        , heading : entity.terrainPosition.heading
        };

    return { op: Protocol.op.SMSG_MOVEPATH, content: content };
}


export function
SMSG_CHANGE_CHARACTER
( player : Player
): M.SMSG_CHANGE_CHARACTER {
    return new M.SMSG_CHANGE_CHARACTER
        ( player.accountId
        , player.roleId
        , player.ready
        );
}


export function
SMSG_JOIN
( party  : Party
, player : Player
): M.SMSG_JOIN {
    return new M.SMSG_JOIN
        ( player.accountId
        , party.id
        , player.roleId
        , player.ready
        , player.entityId
        , player.isConnected
        );
}
