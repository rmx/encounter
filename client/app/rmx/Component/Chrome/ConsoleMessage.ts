/// <reference path="../Base.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Game/events.ts" />
/// <reference path="../../Pure/Template.ts" />

module rmx.Component.Chrome {

    export interface ConsoleMessageProps {
        client : rmx.Game.Client;
        msg    : rmx.Game.ConsoleMessage;
    }

    class ConsoleMessageSpec extends ReactComponent<ConsoleMessageProps, {}> {

        render() {
            var client = this.props.client
              , msg    = this.props.msg
              , style  = msg.color ? { color: msg.color } : undefined;

            return React.DOM.div
                ( { className: 'event', style: style }
                , renderTemplate(client.state, msg.template)
                );
        }
    }


    var fallbackIconUrl = "https://0.gravatar.com/avatar/7f02c799d087a8fa2cc3d4b25bf0d2e3";

    export function
    renderTemplate(state: rmx.Game.State, t: rmx.Pure.Template.Template) {
        return t.map((f, key) => {
            var c     = f.content
              , style = <any> (f.color ? { style: f.color } : {});

            if (typeof c === "string") {
                return React.DOM.span({ key: key, style: style }, c);

            } else if (c instanceof rmx.Pure.Template.IconF) {
                var src = rmx.data.resolveReference(c.ref)
                    .fmap(rmx.data.objectIconUrl).get(fallbackIconUrl);

                return React.DOM.img({ key: key, style: style, src: src, className: 'consoleMessageIcon' });

            } else if (c instanceof rmx.Pure.Template.EntityF) {
                var entity = rmx.Pure.lookupEntity<rmx.Game.WorldObject>(state, c.entityId);
                return React.DOM.span({ key: key, style: style }, (entity ? entity.name : null) || '<' + c.entityId + '>');

            } else if (c instanceof rmx.Pure.Template.PlayerF) {
                var player = rmx.Pure.lookupPlayer(state, c.accountId)
                  , pe = rmx.Pure.lookupEntity<rmx.Game.WorldObject>(state, player.entityId);

                style.fontWeight = 700;

                if (pe) {
                    return React.DOM.span({ key: key, style: style }, pe.name);
                } else {
                    var login = rmx.Core.accountLogin(c.accountId);
                    return React.DOM.span({ key: key, style: style }, login);
                }

            } else {
                return React.DOM.span({ key: key, style: style }, '<UnknownFragment ' + f + '>');
            }
        });
    }

    export var ConsoleMessage = createClass(ConsoleMessageSpec);
}
