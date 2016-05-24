/// <reference path="./Base.ts" />
/// <reference path="../Game/Client.ts" />

declare var classNames;

module rmx.Component {

    var ReactCSSTransitionGroup = React.addons.CSSTransitionGroup;


    export interface ScrollingCombatLogProps {
        client : rmx.Game.Client;
    }

    class ScrollingCombatLogSpec extends ReactComponent<ScrollingCombatLogProps, {}> {
        render() {
            var client = this.props.client
              , state  = client.state;

            var values = rmx.Game.combatEvents(state).filter(x => {
                return (rmx.now() - x.time) < 2 && (x.event.source instanceof rmx.Pure.Spell) &&
                       (x.event.entityId == client.controlledEntityId ||
                       (<rmx.Pure.Spell>x.event.source).casterId == client.controlledEntityId);
            }).map(x => {
                var ce     = x.event
                  , source = <rmx.Pure.Spell> ce.source
                  , value  = (<rmx.Pure.Game.Damage|rmx.Pure.Game.Heal>ce.event).amount;

                var className = classNames(
                    { rmx : 1
                    , "game-scrolling-combatlog" : 1
                    , heal_out : (ce.event instanceof rmx.Pure.Game.Heal &&
                                source.casterId == client.controlledEntityId)
                    , heal_inc : (ce.event instanceof rmx.Pure.Game.Heal &&
                                ce.entityId == client.controlledEntityId)
                    , dmg_inc : (ce.event instanceof rmx.Pure.Game.Damage &&
                                ce.entityId == client.controlledEntityId)
                    , dmg_out : (ce.event instanceof rmx.Pure.Game.Damage &&
                                source.casterId == client.controlledEntityId)
                    });

                // Events don't have a unique key, but we're cheating by using
                // the event timestamp.
                return React.DOM.div
                    ( { className: className, key: x.time }
                    , value
                    );
            });

            return React.DOM.div
                ( {}
                , React.createElement(ReactCSSTransitionGroup
                    , { transitionName: 'combatlog' }
                    , values
                    )
                );
        }
    }

    export var ScrollingCombatLog = createClass(ScrollingCombatLogSpec);
}
