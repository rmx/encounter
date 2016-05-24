/// <reference path="../Base.ts" />
/// <reference path="../../Game.ts" />

module rmx.Component.Chrome {

    export interface GameFinishedOverlayProps {
        client : rmx.Game.Client;
    }

    class GameFinishedOverlaySpec extends ReactComponent<GameFinishedOverlayProps, {}> {
        render() {
            var client = this.props.client
              , state  = client.state
              , party  = rmx.Pure.playerParty(state, rmx.data.session.accountId);

            var outcome = 'Your party lost the game';
            if (rmx.Pure.objectivesCompleted(party)) {
                outcome = 'Your party won the game';
            }

            var encounterHref;
            if (rmx.Core.isTutorialEncounter(state.gameId)) {
                encounterHref = '/survey';
            } else {
                encounterHref = '/games/' + state.gameId + '/after';
            }

            return React.DOM.div
                ( { className: 'game-finished' }
                , React.DOM.div({ className: 'headline' }, outcome)
                , React.DOM.div
                    ( { className: 'link' }
                    , Link({ href: encounterHref}, 'Continue')
                    )
                );
        }
    }

    export var GameFinishedOverlay = createClass(GameFinishedOverlaySpec);
}
