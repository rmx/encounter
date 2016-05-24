/// <reference path="../State.ts" />
/// <reference path="../../time.ts" />


module rmx.Game.Decoder.State {

    function
    withEntity<T extends rmx.Pure.IEntity>
    ( state    : rmx.Game.State
    , entityId : rmx.Pure.EntityId
    , func     : (entity: T) => void
    ): void {
        var entity = rmx.Pure.lookupEntity<T>(state, entityId);
        if (entity) {
            func(entity);
        } else {
            console.warn('Entity ' + entityId + ' does not exist');
        }
    }


    export function
    SMSG_CREATE_WORLDOBJECT(state: rmx.Game.State, json): void {
        var entity = WorldObject.parseJSON(json);

        for (var powerType in json.powers) {
            entity.powers.set(powerType, json.powers[powerType]);
        }

        json.auras.forEach(aura => {
            entity.auras[aura.slot] = rmx.Pure.Aura.parseJSON(aura);
        });

        rmx.Pure.registerEntity(state, entity);
    }


    export function
    SMSG_CREATE_TERRAIN(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_CREATE_TERRAIN.parseJSON(json);
        rmx.Pure.registerEntity(state, msg.terrain);
    }


    export function
    SMSG_CREATE_GROUNDAREAEFFECT(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_CREATE_GROUNDAREAEFFECT.parseJSON(json);
        rmx.Pure.registerEntity(state, msg.gae);
    }


    export function
    SMSG_CREATE_PROJECTILE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_CREATE_PROJECTILE.parseJSON(json);

        // Override the createdAt time. This is needed because the server and
        // client times aren't synchronized yet, the client uses local time
        // exclusively.
        msg.projectile.createdAt = rmx.now();

        rmx.Pure.registerEntity(state, msg.projectile);
    }


    export function
    SMSG_ENTITY_UPDATE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_ENTITY_UPDATE.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.updateFields(msg.fields);
        });
    }

    export function
    SMSG_ENTITY_REMOVE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_DESTROY_ENTITY.parseJSON(json);
        rmx.Pure.destroyEntity(state, msg.entityId);
    }


    export function
    SMSG_ENTITY_POWER(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_ENTITY_POWER.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.powers.get(msg.powerType).value = msg.value;
        });
    }


    export function
    SMSG_UPDATE_AURA(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_UPDATE_AURA.parseJSON(json);
        withEntity<WorldObject>(state, msg.aura.holderId, entity => {
            entity.auras[msg.aura.slot] = msg.aura;
        });
    }


    export function
    SMSG_REMOVE_AURA(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_REMOVE_AURA.parseJSON(json);
        withEntity<WorldObject>(state, msg.holderId, entity => {
            delete entity.auras[msg.slot];
        });
    }


    export function
    SMSG_MOVE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_MOVE.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.moveFlags       = msg.moveFlags;
            entity.terrainPosition = msg.position;
        });
    }


    export function
    SMSG_TELEPORT(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_TELEPORT.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.moveFlags       = 0;
            entity.terrainPosition = msg.position;
        });
    }


    export function
    SMSG_MOVEPATH(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_MOVEPATH.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.movePath = msg.movePath;
        });
    }


    export function
    SMSG_SPELLCAST_START(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_SPELLCAST_START.parseJSON(json);
        withEntity<WorldObject>(state, msg.casterId, entity => {
            entity.spell = new rmx.Pure.Spell
                ( msg.id
                , msg.casterId
                , msg.spellId
                , msg.spellTarget
                , rmx.now()
                , msg.castTime
                , null
                , null
                );

            pushConsoleMessage
                ( state
                , rmx.now()
                , undefined
                , rmx.Game.Events.spellCastStartTemplate(msg)
                );
        });
    }


    export function
    SMSG_SPELLCAST_CANCEL(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_SPELLCAST_CANCEL.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            entity.spell = null;
        });
    }


    export function
    SMSG_SPELLCAST_FINISH(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_SPELLCAST_FINISH.parseJSON(json);
        withEntity<WorldObject>(state, msg.entityId, entity => {
            if (entity.spell) {
                pushAnnotatedEvent
                    ( state
                    , new rmx.Game.SpellcastFinish
                        ( msg.entityId
                        , entity.spell
                        , msg.targetInfos
                        )
                    );

                entity.spell = null;
            }
        });
    }


    export function
    SMSG_CHATMESSAGE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_CHATMESSAGE.parseJSON(json);
        // TODO: Translate the msg directly to a ChatMessageEvent.

        pushConsoleMessage
            ( state
            , rmx.now()
            , undefined
            , rmx.Game.Events.chatMessageTemplate(msg.senderId, msg.text)
            );
    }


    export function
    SMSG_METADATA(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_METADATA.parseJSON(json);
        state.encounterId = msg.encounterId;
    }


    export function
    SMSG_UPDATE_OBJECTIVE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_UPDATE_OBJECTIVE.parseJSON(json);

        var party = rmx.Pure.lookupParty(state, msg.partyId);
        if (!party) {
            party = new rmx.Pure.Party(msg.partyId);
            state.parties.push(party);
        }

        var objective = party.objectives.filter(x => { return x.id === msg.objectiveId; })[0];
        if (!objective) {
            objective = new rmx.Pure.Objective(msg.objectiveId, []);
            party.objectives.push(objective);
        }

        objective.isCompleted = msg.isCompleted;
    }


    export function
    SMSG_JOIN(state: rmx.Game.State, json): void {
        var msg    = rmx.Pure.Message.SMSG_JOIN.parseJSON(json)
          , player = new rmx.Pure.Player(msg.accountId, msg.roleId, null, msg.ready, msg.isConnected);

        player.entityId = msg.controlledEntityId;

        var party = rmx.Pure.lookupParty(state, msg.partyId);
        if (party) {
            party.players.push(player);
        } else {
            party = new rmx.Pure.Party(msg.partyId);
            state.parties.push(party);
            party.players.push(player);
        }
    }


    export function
    SMSG_CHANGE_PARTY(state: rmx.Game.State, json): void {
        var msg    = rmx.Pure.Message.SMSG_CHANGE_PARTY.parseJSON(json)
          , player = rmx.Pure.lookupPlayer(state, msg.accountId);

        state.parties.forEach(party => {
            party.remove(msg.accountId);
        });

        var party = rmx.Pure.lookupParty(state, msg.partyId);
        if (party) {
            party.players.push(player);
        } else {
            party = new rmx.Pure.Party(msg.partyId);
            state.parties.push(party);
            party.players.push(player);
        }
    }


    export function
    SMSG_CHANGE_CHARACTER(state: rmx.Game.State, json): void {
        var msg    = rmx.Pure.Message.SMSG_CHANGE_CHARACTER.parseJSON(json)
          , player = rmx.Pure.lookupPlayer(state, msg.accountId);

        player.roleId  = msg.roleId;
        player.ready   = msg.ready;
    }


    export function
    SMSG_COMBAT_EVENT(state: rmx.Game.State, json): void {
        var ce  = rmx.Pure.Game.CombatEvent.parseJSON(json)
          , res = rmx.Game.Events.combatEventTemplate(ce);

        pushConsoleMessage
            ( state
            , rmx.now()
            , res.color
            , res.template
            );

        pushAnnotatedEvent(state, ce);
    }


    export function
    SMSG_SCORE(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_SCORE.parseJSON(json);

        var party = rmx.Pure.lookupParty(state, msg.partyId);
        if (party) {
            party.score = msg.score;
        } else {
            party = new rmx.Pure.Party(msg.partyId);
            state.parties.push(party);
            party.score = msg.score;
        }
    }


    export function
    SMSG_DEBUG(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_DEBUG.parseJSON(json);

        pushConsoleMessage
            ( state
            , rmx.now()
            , undefined
            , rmx.Game.Events.debugTemplate(msg.text)
            );
    }


    export function
    SMSG_SAY(state: rmx.Game.State, json): void {
        var msg = rmx.Pure.Message.SMSG_SAY.parseJSON(json);

        pushConsoleMessage
            ( state
            , rmx.now()
            , undefined
            , rmx.Game.Events.sayTemplate(msg)
            );
    }


    export function
    SMSG_CHANGE_STAGE(state: rmx.Game.State, json, link: Link): void {
        var msg = rmx.Pure.Message.SMSG_CHANGE_STAGE.parseJSON(json);
        state.stage = msg.stage;
    }
}
