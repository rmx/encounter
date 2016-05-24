/// <reference path="../Base.ts" />
/// <reference path="./ActionButton.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface ActionBarProps {
        client : rmx.Game.Client;
    }

    class ActionBarSpec extends ReactComponent<ActionBarProps, {}> {
        render() {
            var client = this.props.client;

            var actionButtons = client.actionButtons.map((action, index) => {
                return ActionButton(
                    { client: client, action: action, index: index + 1, key: index });
            });

            return React.DOM.div
                ( { className: 'action bar' }
                , actionButtons
                );
        }
    }

    export var ActionBar = createClass(ActionBarSpec);
}
