/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface PartyProps {
        client : rmx.Game.Client;
    }

    class PartySpec extends ReactComponent<PartyProps, {}> {
        render() {
            var client = this.props.client;
            var controlledEntityId = client.controlledEntityId;

            var teamMembers = client.party.players.filter(player => {
                return player.entityId != controlledEntityId;
            }).map(player => {
                return partyMemberFrame(client, player);
            });

            return React.DOM.div({ className: 'team frame' }, teamMembers);
        }
    }

    export var Party = createClass(PartySpec);


    function
    partyMemberFrame(client: rmx.Game.Client, player: rmx.Pure.Player) {
        var unit = rmx.Pure.lookupEntity<rmx.Game.WorldObject>(client.state, player.entityId);
        if (unit) {
            var auras = unit.auraList.map(aura => {
                return auraIcon(aura);
            });

            return React.DOM.div
                ( { className: 'party-member-frame' }

                , React.DOM.div({ className: 'bar', style: { width: unit.healthPercent + '%' }})

                , React.DOM.div
                    ( { className: 'overlay' }
                    , React.DOM.div({ className: 'health-amount-label' }, rmx.Core.humanReadableAmount(unit.health))
                    , React.DOM.div({ className: 'name' }, unit.name)
                    , React.DOM.div({ className: 'health-percent-label' }, Math.round(unit.healthPercent) + '%')
                    )

                , React.DOM.div
                    ( { className: 'auras' }
                    , auras
                    )
                );
        }
    }
}
