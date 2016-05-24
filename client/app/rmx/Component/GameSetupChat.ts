/// <reference path="./Base.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./Link.ts" />

/// <reference path="../data.ts" />
/// <reference path="../Core/Encounter.ts" />
/// <reference path="../Core/Game.ts" />
/// <reference path="../Game/Client.ts" />

/// <reference path="./Chrome/ConsoleMessage.ts" />

module rmx.Component {

    export interface GameSetupChatProps {
        client : rmx.Game.Client;
    }

    export interface GameSetupChatState {
        input : string;
    }

    class GameSetupChatSpec extends ReactComponent<GameSetupChatProps, GameSetupChatState> {

        getInitialState() {
            return { input: '' };
        }

        render() {
            var client = this.props.client
              , state  = client.state
              ;

            var messages = state.consoleMessages.map(x => {
                return React.DOM.div
                    ( { className: 'rmx game-setup-chat-message' }
                    , Chrome.renderTemplate(state, x.template)
                    );
            });

            return React.DOM.form
                ( { className: 'rmx game-setup-messages', onSubmit: this.submit }
                , React.DOM.div
                    ( { className: 'messages' }
                    , React.DOM.div({ className: 'message-list' }, messages)
                    )
                , React.DOM.input({ className: 'input', placeholder: 'Write a message here and press enter', value: this.state.input, onChange: this.changeInput })
                );
        }

        changeInput(e) {
            this.setState({ input: e.target.value });
        }

        submit(e) {
            e.preventDefault();

            var input = this.state.input;
            if (input.length > 0) {
                rmx.Game.sendChatMessage(this.props.client, input);
                this.setState({ input: '' });
            }
        }
    }

    export var GameSetupChat = createClass(GameSetupChatSpec);
}
