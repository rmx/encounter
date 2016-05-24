/// <reference path="./Base.ts" />
/// <reference path="../Game/Client.ts" />

module rmx.Component {

    export interface GameHeaderProps {
        client : rmx.Game.Client;
    }

    function pad(text: any, length: number): string {
        return ("00000000" + text).slice(-length);
    }

    class GameHeaderSpec extends ReactComponent<GameHeaderProps, {}> {
        render() {
            var client   = this.props.client
              , state    = client.state
              , gameHref = '/games/' + state.gameId
              , logHref  = '/games/' + state.gameId + '/log'
              , game     = rmx.data.findById<rmx.Storage.Game>(client.gameId);

            var encounterItem = game.fmap(x => {
                if (x.content.encounter.objectId === rmx.config.tutorialEncounterId) {
                    return React.DOM.a
                        ( { className: 'item'
                          , href: '/survey'
                          }
                        , 'Exit'
                        );

                } else if (x.content.purpose === 'grading') {
                    return React.DOM.a
                        ( { className: 'item'
                          , href: '/e/' + x.content.encounter.objectId
                          }
                        , 'Exit'
                        );

                } else {
                    return React.DOM.a
                        ( { className: 'item'
                          , href: '/o/' + x.content.encounter.objectId
                          }
                        , 'Editor'
                        );
                }
            }).get(null);

            var parties = state.parties.map(party => {
                var isCompleted  = rmx.Pure.objectivesCompleted(party)
                  , iconClass    = isCompleted ? 'green' : 'red';

                return React.DOM.div
                    ( { className: 'item', key: party.id }
                    , React.DOM.i({ className: 'icon circle ' + iconClass })
                    , party.id
                    , ' ('
                    , rmx.Pure.numCompletedObjectives(party)
                    , '/'
                    , party.objectives.length
                    , ')'
                    );
            });

            var now     = rmx.now()
              , diff    = now - client.createdAt
              , minutes = Math.floor(diff / 60)
              , seconds = Math.floor(diff - minutes * 60);

            var time = React.DOM.div
                ( { className: 'time' }
                , pad(minutes, 2) + ':' + pad(seconds, 2)
                );

            var fps = React.DOM.div
                ( { className: 'fps' }
                , pad(Math.round(client.stats.fps.value), 2), 'fps'
                );

            var wsStatusClass      = client.link.ready ? 'green' : 'red'
              , webSocketIndicator = React.DOM.div
                ( {}
                , React.DOM.i({ className: 'icon circle ' + wsStatusClass })
                , 'Network'
                );

            return React.DOM.div
                ( { className: 'rmx game-header' }
                , encounterItem
                , React.DOM.a({ className: 'item', href: gameHref }, 'Game')
                , React.DOM.a({ className: 'item', href: logHref }, 'Log')
                , React.DOM.div({ className: 'item' }, 'Parties: ')
                , parties
                , React.DOM.div({ className: 'expander item' })
                , navbarPeers()
                , React.DOM.div({ className: 'item' }, time)
                , React.DOM.div({ className: 'item' }, fps)
                , React.DOM.div({ className: 'item' }, webSocketIndicator)
                );
        }
    }

    export var GameHeader = createClass(GameHeaderSpec);
}
