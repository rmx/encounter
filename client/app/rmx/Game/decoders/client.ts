/// <reference path="../Client.ts" />


module rmx.Game.Decoder.Client {

    export function
    NULL(client: rmx.Game.Client, json): void {
        rmx.Pure.Message.NULL.parseJSON(json);
    }


    export function
    SMSG_PROFILE(client: rmx.Game.Client, json): void {
        // This is the response to AUTHENTICATE. We ignore the message
        // because we already have the full 'Account' object.
        //
        // The only thing that may be useful is the gravatar url.
    }


    export function
    SMSG_CONTROL(client: rmx.Game.Client, json): void {
        var msg    = rmx.Pure.Message.SMSG_CONTROL.parseJSON(json)
          , player = rmx.Pure.lookupPlayer(client.state, msg.accountId);

        player.entityId = msg.entityId;
    }


    export function
    SMSG_INITIAL_SPELLS(client: rmx.Game.Client, json): void {
        var msg = rmx.Pure.Message.SMSG_INITIAL_SPELLS.parseJSON(json);

        msg.spells.forEach(spellInfo => {
            client.spells.set(spellInfo.id, spellInfo);

            var ref = rmx.Storage.Reference.parseString(spellInfo.id);
            client.actionButtons.push(new rmx.Game.SpellAction(spellInfo, ref));
        });


    }


    export function
    SMSG_SPELLCAST_FAILED(client: rmx.Game.Client, json): void {
        // Empty.
    }


    export function
    SMSG_SPELL_COOLDOWNS(client: rmx.Game.Client, json) {
        var msg = rmx.Pure.Message.SMSG_SPELL_COOLDOWNS.parseJSON(json);

        var entity = client.controlledEntity;
        if (entity) {
            entity.spellCooldowns.clear();
            msg.spells.forEach(sce => {
                var timer = new rmx.Pure.CooldownTimer(sce.start, sce.duration);
                entity.spellCooldowns.set(sce.spellId, timer);
            });
        } else {
            // XXX: Server error. It should not send this message when the
            // client doesn't have a controlled entity.
        }
    }


    export function
    SMSG_DISCONNECT(client: rmx.Game.Client, json, link: Link): void {
        var msg = rmx.Pure.Message.SMSG_DISCONNECT.parseJSON(json);
        console.log('Received a DISCONNECT message from the server', msg);

        client.error = new Error(msg.msg);
    }
}
