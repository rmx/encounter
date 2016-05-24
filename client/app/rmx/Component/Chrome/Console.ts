/// <reference path="../Base.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Game/events.ts" />

module rmx.Component.Chrome {

    export interface ConsoleProps {
        client : rmx.Game.Client;
    }

    class ConsoleSpec extends ReactComponent<ConsoleProps, {}> {

        render() {
            var client = this.props.client;

            var events = client.state.consoleMessages.slice(-10).map(msg => {
                return ConsoleMessage({ client: client, msg: msg, key: msg.time });
            });

            return React.DOM.div
                ( { className: 'console frame' }
                , React.DOM.div({ className: 'events' }, events)
                );
        }

    }

    export var Console = createClass(ConsoleSpec);
}
