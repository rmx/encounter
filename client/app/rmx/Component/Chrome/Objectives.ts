/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface ObjectivesProps {
        client : rmx.Game.Client;
    }

    class ObjectivesSpec extends ReactComponent<ObjectivesProps, {}> {
        render() {
            var client = this.props.client
              , state  = client.state
              , party  = rmx.Pure.playerParty(state, rmx.data.session.accountId);

            var objectives = party.objectives.map(function(x) {
                return Objective({ client: client, objective: x, key: x.id });
            });

            return React.DOM.div
                ( { className: 'rmx objectives-frame' }
                , objectives
                );
        }
    }

    export var Objectives = createClass(ObjectivesSpec);
}
