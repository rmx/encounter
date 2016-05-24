/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface UnitFrameProps {
        client : rmx.Game.Client;
        unit   : rmx.Game.WorldObject;
    }

    class UnitFrameSpec extends ReactComponent<UnitFrameProps, {}> {
        render() {
            var unit = this.props.unit;
            if (unit) {
              var playerName     = unit.name
                , powerBarLabel  = Math.round(unit.powerPercent)
                ;

                return React.DOM.div
                    ( { className: 'rmx unit-frame', onClick: this.selectUnit }

                    , React.DOM.div
                        ( { className: 'mana-bar' }
                        , React.DOM.div({ className: 'bar', style: { width: unit.powerPercent + '%' }})
                        , React.DOM.div({ className: 'mana-bar-label' }, powerBarLabel)
                        )

                    , React.DOM.div({ className: 'bar', style: { width: unit.healthPercent + '%' }})

                    , React.DOM.div
                        ( { className: 'overlay' }
                        , React.DOM.div({ className: 'health-amount-label' }, rmx.Core.humanReadableAmount(unit.health))
                        , React.DOM.div({ className: 'name' }, playerName)
                        , React.DOM.div({ className: 'health-percent-label' }, Math.round(unit.healthPercent) + '%')
                        )
                    );
            } else {
                return React.DOM.div();
            }
        }

        selectUnit() {
            rmx.Game.selectEntity(this.props.client, this.props.unit.id);
        }
    }

    export var UnitFrame = createClass(UnitFrameSpec);
}
