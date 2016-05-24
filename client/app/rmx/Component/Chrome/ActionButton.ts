/// <reference path="../Base.ts" />
/// <reference path="../TooltipMixin.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Core/Spell.ts" />

module rmx.Component.Chrome {

    var fallbackIconUrl = "https://0.gravatar.com/avatar/7f02c799d087a8fa2cc3d4b25bf0d2e3";


    export interface ActionButtonProps {
        client : rmx.Game.Client;
        action : rmx.Game.Action;
        index  : number;
    }

    class ActionButtonSpec extends ReactComponent<ActionButtonProps, {}> {
        render() {
            var client = this.props.client
              , action = this.props.action
              , index  = this.props.index;

            var res     = rmx.data.resolveReference(action.spellRef);
            var iconUrl = res.fmap(rmx.data.objectIconUrl).get(fallbackIconUrl);

            var className = "rmx action-button";
            var ce = client.controlledEntity;
            if (ce && !rmx.Game.isSpellReady(client, ce, action.spellRef).get(true)) {
                className += " disabled";
            }

            var overlayText = null;

            var cooldownLeft = rmx.Game.actionCooldownLeft(client, action);
            if (cooldownLeft > 0) {
                className = className + " cooldown";
                var rounded = Math.round(cooldownLeft);

                if (Math.round(cooldownLeft * 10) < 10) {
                    overlayText = '.' + (10 * cooldownLeft).toFixed(0);
                } else if (rounded < 100) {
                    overlayText = '' + rounded;
                } else {
                    overlayText = Math.round(cooldownLeft / 60) + 'm';
                }
            }

            var keyBinding =
                client.bindings.getBindingsFor('action-button-' + index)[0];

            var label = '';

            return React.DOM.div
                ( { className: className, onClick: this.useAction }
                , React.DOM.img({ src: iconUrl })
                , React.DOM.div({ className: 'label' }, label)
                , React.DOM.div
                    ( { className: 'overlay' }
                    , React.DOM.div({ className: 'text' }, overlayText)
                    )
                , React.DOM.div({ className: 'key' } , keyBinding)
                );
        }

        useAction() {
            rmx.Game.useAction(this.props.client, this.props.action);
        }

        tooltipContent() {
            var action = this.props.action;

            var displayName = rmx.data.displayName(action.spellRef).get(action.spellInfo.id);

            var summary = rmx.Core.resourceContentReference<rmx.Storage.Spell>(action.spellRef).fmap(s => {
                return rmx.Core.spellSummary(s);
            }).get('');

            return React.DOM.div
                ( {}
                , displayName
                , React.DOM.p({}, summary)
                );
        }
    }

    ActionButtonSpec.prototype.mixins =
        [ TooltipMixin
        ];


    export var ActionButton = createClass(ActionButtonSpec);
}
