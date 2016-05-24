import { Projectile } from '../Pure/Game';
import { WorldObject, moveTo, setTargetId } from '../Game/Entities/WorldObject';


import { AccountId } from '../Pure/Ids';
import { lookupPlayer, playerControlledEntity, Player, playerParty, lookupParty, Stage } from '../Pure/Game';
import { Terrain } from '../Pure/Terrain';
import { State } from '../Game/State';
import { broadcastMessage, messageToAccount, broadcastMessageSkip, transitionToRunning,
    createPlayerEntity } from '../Game';
import { attemptSpellcast, abortSpellcast } from '../Game/Spell';
import { SMSG_MOVEPATH, SMSG_SPELL_COOLDOWNS, SMSG_JOIN, SMSG_CHANGE_CHARACTER,
    SMSG_CONTROL, SMSG_INITIAL_SPELLS,
    SMSG_CREATE_WORLDOBJECT, SMSG_CREATE_GROUNDAREAEFFECT } from '../Game/Messages';
import * as Message from '../Pure/Message';
import { GroundAreaEffect } from '../Game/Entities/GroundAreaEffect';



// -----------------------------------------------------------------------
export function
CMSG_PING(state: State, accountId: AccountId, json) {
    var msg = Message.NULL.parseJSON(json);
    messageToAccount(state, accountId, new Message.SMSG_PONG(msg.cookie));
}



// -----------------------------------------------------------------------
export function
CONNECT(state: State, accountId: AccountId) {
    var encounterId = state.contentProvider.encounterId;

    messageToAccount(state, accountId, new Message.SMSG_METADATA(encounterId));
    messageToAccount(state, accountId, new Message.SMSG_CHANGE_STAGE(state.stage));

    // Disconnect the client if the game is past the setup stage and the player
    // is not in the party.
    if (state.stage !== Stage.Setup && !lookupPlayer(state, accountId)) {
        var msg = new Message.SMSG_DISCONNECT('Game is already running');
        messageToAccount(state, accountId, msg);

        return;
    }


    // Send data about parties, players and objectives.
    state.parties.forEach(party => {
        messageToAccount(state, accountId, new Message.SMSG_SCORE(party.id, party.score));

        party.players.forEach(player => {
            messageToAccount(state, accountId, SMSG_JOIN(party, player));
        });

        party.objectives.forEach(objective => {
            var msg = new Message.SMSG_UPDATE_OBJECTIVE(party.id, objective.id, objective.isCompleted);
            messageToAccount(state, accountId, msg);
        });
    });


    // If the player is not present in any of the parties (ie. he just
    // joined), add the player to the first party and send SMSG_JOIN to
    // everybody.
    var player = lookupPlayer(state, accountId);
    if (player) {
        player.isConnected = true;

    } else {
        player = new Player(accountId, null, null, null, true);
        state.parties[0].players.push(player);
        broadcastMessage(state, SMSG_JOIN(state.parties[0], player));
    }


    // First send all terrains over. That is probably because the client
    // can't handle having a TerrainPosition (such as on a WorldObject)
    // but no terrain for it yet.
    state.entities.forEach(x => {
        if (x instanceof Terrain) {
            var msg = new Message.SMSG_CREATE_TERRAIN(x);
            messageToAccount(state, accountId, msg);
        }
    });


    // Then all other entities.
    state.entities.forEach((x: any) => {
        if (x instanceof GroundAreaEffect) {
            messageToAccount(state, accountId, SMSG_CREATE_GROUNDAREAEFFECT(x));

        } else if (x instanceof WorldObject) {
            messageToAccount(state, accountId, SMSG_CREATE_WORLDOBJECT(x));
            messageToAccount(state, accountId, SMSG_MOVEPATH(x));

        } else if (x instanceof Projectile) {
            messageToAccount(state, accountId, new Message.SMSG_CREATE_PROJECTILE(x));

        }
    });


    // If players have an entity they control, send info about that.
    state.parties.forEach(party => {
        party.players.forEach(player => {
            if (player.entityId) {
                messageToAccount(state, accountId, SMSG_CONTROL(player));
            }
        });
    });


    // If we have a entity for the player, send it to the client.
    var entity = playerControlledEntity<WorldObject>(state, accountId);
    if (entity) {
        messageToAccount(state, accountId, SMSG_INITIAL_SPELLS(entity));
        messageToAccount(state, accountId, SMSG_SPELL_COOLDOWNS(entity));
    }
}


// -----------------------------------------------------------------------
export function
CMSG_MOVE(state: State, accountId: AccountId, json) {
    var msg    = Message.CMSG_MOVE.parseJSON(json)
      , entity = playerControlledEntity<WorldObject>(state, accountId);

    if (entity) {
        // TODO: Validate movement.
        moveTo(state, entity, msg.position, msg.moveFlags);

        msg = new Message.SMSG_MOVE(entity.id, msg.position, msg.moveFlags);
        broadcastMessageSkip(state, entity, msg);
    }
}



// -----------------------------------------------------------------------
export function
CMSG_CHATMESSAGE(state: State, accountId: AccountId, json) {
    var msg = Message.CMSG_CHATMESSAGE.parseJSON(json);
    broadcastMessage(state, new Message.SMSG_CHATMESSAGE(accountId, msg.text));
}



// -----------------------------------------------------------------------
export function
SMSG_DISCONNECT(state: State, accountId: AccountId) {
    var player = lookupPlayer(state, accountId);
    if (player) {
        player.isConnected = false;
    }
}



// -----------------------------------------------------------------------
export function
TARGET(state: State, accountId: AccountId, json) {
    var msg    = Message.CMSG_TARGET.parseJSON(json)
      , entity = playerControlledEntity<WorldObject>(state, accountId);

    if (entity) {
        setTargetId(state, entity, msg.id);
    }
}



// -----------------------------------------------------------------------
export function
CMSG_SPELLCAST(state: State, accountId: AccountId, json) {
    var msg    = Message.CMSG_SPELLCAST.parseJSON(json)
      , entity = playerControlledEntity<WorldObject>(state, accountId);

    if (entity) {
        attemptSpellcast(state, entity, msg.spellTarget, msg.spellId);
    }
}



// -----------------------------------------------------------------------
export function
CMSG_SPELLCAST_ABORT(state: State, accountId: AccountId) {
    var entity = playerControlledEntity<WorldObject>(state, accountId);

    if (entity) {
        abortSpellcast(state, entity);
    }
}



// -----------------------------------------------------------------------
export function
TICK() {
}



// -----------------------------------------------------------------------
export function
CMSG_INVITE(state: State, accountId: AccountId, json) {
    // FIXME: Hack to silence unused variable warning.
    (() => { return accountId; })();

    var msg = Message.CMSG_INVITE.parseJSON(json);

    if (!lookupPlayer(state, msg.accountId)) {
        var player = new Player(msg.accountId, null, null, null, false);
        state.parties[0].players.push(player);

        broadcastMessage(state, SMSG_JOIN(state.parties[0], player));
    }
}



// -----------------------------------------------------------------------
export function
CMSG_CHANGE_PARTY(state: State, accountId: AccountId, json) {
    var msg = Message.CMSG_CHANGE_PARTY.parseJSON(json);

    // Only do work if the player actually is in a different party.
    if (playerParty(state, accountId).id !== msg.partyId) {
        var player = lookupPlayer(state, accountId);

        state.parties.forEach(party => {
            party.remove(accountId);
        });

        var party = lookupParty(state, msg.partyId);
        party.players.push(player);

        broadcastMessage(state, new Message.SMSG_CHANGE_PARTY(accountId, party.id));

        // Changing the party resets the character class which the player has
        // chosen.
        player.roleId = null;
        player.ready  = false;

        broadcastMessage(state, SMSG_CHANGE_CHARACTER(player));
    }
}



// -----------------------------------------------------------------------
export function
CMSG_CHANGE_CHARACTER(state: State, accountId: AccountId, json) {
    var msg    = Message.CMSG_CHANGE_CHARACTER.parseJSON(json)
      , player = lookupPlayer(state, accountId);

    player.ready  = false;
    player.roleId = msg.roleId;

    broadcastMessage(state, SMSG_CHANGE_CHARACTER(player));
}



// CMSG_CONFIRM_CHARACTER_CHOICE
// -----------------------------------------------------------------------
//
// This message is only valid when the game is in the Setup stage. However
// due to race conditions, it is possible for the client to send it while
// we are already Running or Finished. If that happens, the server
// silently ignores the message.

export function
CMSG_CONFIRM_CHARACTER_CHOICE(state: State, accountId: AccountId) {
    if (state.stage === Stage.Setup) {
        var player = lookupPlayer(state, accountId);

        if (player.roleId) {
            player.ready = true;
        }

        broadcastMessage(state, SMSG_CHANGE_CHARACTER(player));

        function allPlayersReady() {
            var ret = true;
            state.parties.forEach(party => {
                party.players.forEach(function(x: Player) {
                    if (!x.ready) {
                        ret = false;
                    }
                });
            });
            return ret;
        }

        if (allPlayersReady()) {
            console.log('All players are ready, transition to Running');

            transitionToRunning(state);
            broadcastMessage(state, new Message.SMSG_CHANGE_STAGE(state.stage));

            // Create entities for each player.
            state.parties.forEach(party => {
                party.players.forEach(player => {
                    createPlayerEntity(state, party, player);
                });
            });

        } else {
            console.log('Not all players are ready yet');
        }
    } else {
        console.log('Got CMSG_CONFIRM_CHARACTER_CHOICE while past Setup stage');
    }
}
