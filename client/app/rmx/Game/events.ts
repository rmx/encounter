/// <reference path="../Pure/Game.ts" />
/// <reference path="../Pure/Game/Events.ts" />
/// <reference path="./Types.ts" />
/// <reference path="./Client.ts" />

module rmx.Game.Events {

    function stringTF(s: string) {
        return new rmx.Pure.Template.TemplateFragment(s, undefined);
    }

    function entityTF(entityId: rmx.Pure.EntityId) {
        return new rmx.Pure.Template.TemplateFragment
            ( new rmx.Pure.Template.EntityF(entityId)
            , undefined
            );
    }

    function playerTF(accountId: rmx.Pure.AccountId) {
        return new rmx.Pure.Template.TemplateFragment
            ( new rmx.Pure.Template.PlayerF(accountId)
            , undefined
            );
    }

    function iconTF(ref: rmx.Storage.Reference) {
        return new rmx.Pure.Template.TemplateFragment
            ( new rmx.Pure.Template.IconF(ref)
            , undefined
            );
    }

    function resourceIconTF(resourceId: string) {
        return rmx.data.parseReferenceString(resourceId).fmap(ref => {
            return iconTF(ref);
        }).get(stringTF("<Icon>"));
    }


    function combatEventSourceTF(event: rmx.Pure.Game.CombatEvent) {
        var source = event.source;

        if (source instanceof rmx.Pure.Game.Environment) {
            return stringTF("Environment");

        } else if (source instanceof rmx.Pure.Aura) {
            return entityTF(source.casterId);

        } else if (source instanceof rmx.Pure.Spell) {
            return entityTF(source.casterId);

        } else {
            return stringTF("<Unknown CombatEvent Source>");
        }
    }


    function combatEventIconTF(event: rmx.Pure.Game.CombatEvent) {
        var source = event.source;

        if (source instanceof rmx.Pure.Game.Environment) {
            // FIXME: What should the environments icon be?
            return stringTF("<Icon>");

        } else if (source instanceof rmx.Pure.Aura) {
            return resourceIconTF(source.auraId);

        } else if (source instanceof rmx.Pure.Spell) {
            return resourceIconTF(source.spellId);

        } else {
            return stringTF("<Unknown CombatEvent Icon>");
        }
    }

    export function
    sayTemplate(msg: rmx.Pure.Message.SMSG_SAY): rmx.Pure.Template.Template {
        return [ entityTF(msg.entityId)
               , stringTF(' says: ')
               , stringTF(msg.message)
               ];
    }

    export function
    debugTemplate(text: string): rmx.Pure.Template.Template {
        return [ stringTF(text) ];
    }

    export function
    chatMessageTemplate(accountId: rmx.Pure.AccountId, text: string): rmx.Pure.Template.Template {
        return [ playerTF(accountId)
               , stringTF(': ')
               , stringTF(text)
               ];
    }

    export function
    spellCastStartTemplate(msg: rmx.Pure.Message.SMSG_SPELLCAST_START): rmx.Pure.Template.Template {
        // TODO: A Spell Fragment would be handy here.
        var spellName = rmx.data.parseReferenceString(msg.spellId).bind(ref => {
            return rmx.data.displayName(ref);
        }).get('a spell');

        return [ entityTF(msg.casterId)
               , stringTF(' is casting ')
               , stringTF(spellName)
               , stringTF(' on ')
               , entityTF(msg.spellTarget.entityId)
               ];
    }


    export function
    combatEventTemplate
    ( ce : rmx.Pure.Game.CombatEvent
    ): { color: string; template: rmx.Pure.Template.Template; } {
        var action = ce.event;

        if (action instanceof rmx.Pure.Game.Heal) {
            return { color: '#12ff00', template:
                   [ combatEventSourceTF(ce)
                   , stringTF(' ')
                   , combatEventIconTF(ce)
                   , stringTF(' ')
                   , entityTF(ce.entityId)
                   , stringTF(' ' + action.amount)
                   ]
            };

        } else if (action instanceof rmx.Pure.Game.Damage) {
            var amount = action.amount
              , school = action.school;

            var overkillTF;
            if (action.overkill) {
                overkillTF = stringTF(' (' + action.overkill + ')');
            }

            var absorbedTF;
            if (action.absorbed) {
                absorbedTF = stringTF(' (' + action.absorbed + ')');
            }

            return { color: '#ff0000', template:
                   [ combatEventSourceTF(ce)
                   , stringTF(' ')
                   , combatEventIconTF(ce)
                   , stringTF(' ')
                   , entityTF(ce.entityId)
                   , stringTF(' ' + amount)
                   , overkillTF
                   , absorbedTF
                   , stringTF(' [' + school.toLowerCase() + ']')
                   ].filter(x => { return x !== undefined; })
            };

        } else if (action instanceof rmx.Pure.Game.Kill) {
            return { color: '#ff0000', template:
                   [ combatEventSourceTF(ce)
                   , stringTF(' killed ')
                   , entityTF(ce.entityId)
                   ]
            };

        } else {
            return { color: '#ff0000', template: [ stringTF("Unhandled CombatEvent") ] };
        }
    }
}
