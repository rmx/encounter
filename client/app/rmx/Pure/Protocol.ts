module rmx.Protocol {

    export module op {

        // Internal opcodes used on the server only.
        export var TICK                            = 0xF013;

        // NULL messages can be sent at any time, they have no effect.
        export var NULL                            = 0x42b3;

        export var CMSG_AUTHENTICATE               = 0x0001;
        export var SMSG_PROFILE                    = 0x1001;

        export var CMSG_PING                       = 0x2201;
        export var SMSG_PONG                       = 0x2202;

        export var SMSG_DEBUG                      = 0x0101;

        export var SMSG_METADATA                   = 0xB6C1;
        export var SMSG_CHANGE_STAGE               = 0x764B;

        export var SMSG_CONTROL                    = 0x0300;
        export var SMSG_MOVE                       = 0x457f;
        export var CMSG_MOVE                       = 0xb7b0;
        export var SMSG_MOVEPATH                   = 0x70E9;
        export var SMSG_TELEPORT                   = 0x1E38;

        export var CMSG_SPELLCAST                  = 0x0401;
        export var CMSG_SPELLCAST_ABORT            = 0x0406;

        export var SMSG_UPDATE_AURA                = 0x65B8;
        export var SMSG_REMOVE_AURA                = 0x8E55;

        export var SMSG_SPELLCAST_START            = 0x0402;
        export var SMSG_SPELLCAST_CANCEL           = 0x0403;
        export var SMSG_SPELLCAST_FAILED           = 0x0408;
        export var SMSG_SPELLCAST_FINISH           = 0x5826;

        export var CMSG_CHATMESSAGE                = 0x28c9;
        export var SMSG_CHATMESSAGE                = 0xfb62;
        export var SMSG_JOIN                       = 0x1004;
        export var SMSG_LEAVE                      = 0x1014;
        export var CMSG_CHANGE_PARTY               = 0x66E8;
        export var SMSG_CHANGE_PARTY               = 0x2F7C;
        export var CMSG_CHANGE_CHARACTER           = 0x42EC;
        export var SMSG_CHANGE_CHARACTER           = 0x6B2C;
        export var CMSG_CONFIRM_CHARACTER_CHOICE   = 0x1007;
        export var SMSG_SAY                        = 0x8c53;

        export var SMSG_UPDATE_OBJECTIVE           = 0xADF2;

        export var SMSG_CREATE_WORLDOBJECT         = 0x100B;
        export var SMSG_CREATE_PROJECTILE          = 0x4925;
        export var SMSG_CREATE_GROUNDAREAEFFECT    = 0x1030;
        export var SMSG_CREATE_TERRAIN             = 0xF010;

        export var SMSG_ENTITY_UPDATE              = 0x1012;
        export var SMSG_ENTITY_POWER               = 0x1013;

        export var SMSG_ENTITY_REMOVE              = 0x000D;

        export var CMSG_TARGET                     = 0x1015;
        export var TARGET                          = 0x1015;

        export var SMSG_SCORE                      = 0x100F;

        export var CONNECT                         = 0x2003;
        export var EXCEPTION                       = 0x2006;

        export var CMSG_INVITE                     = 0x2007;
        export var CMSG_KICK                       = 0x2008;

        export var SMSG_INITIAL_SPELLS             = 0xF006;
        export var SMSG_SPELL_COOLDOWNS            = 0xF008;
        export var SMSG_COMBAT_EVENT               = 0x24C4;

        export var SMSG_DISCONNECT                 = 0xFFFF;
    }


    var opNames = invert(op);
    function invert(obj: any): any {
        var ret = {};

        for (var k in obj) {
            ret[obj[k]] = k;
        }

        return ret;
    }

    export function
    opName(op: number): string {
        var name = opNames[op];
        if (name) {
            return name;
        } else {
            throw new Error('Unknow op: 0x' + op.toString(16));
        }
    }


    export function
    encodeMessage(msg: { op: number; content: any; }, time: number): any {
        return [msg.content, { opcode: msg.op, time: time }].reduce(assign, {});
    }

    function assign(O, dictionary) {
        var target = Object(O)
          , src    = Object(dictionary);

        Object.getOwnPropertyNames(src).forEach(function(key) {
            target[key] = src[key];
        });

        return target;
    }
}
