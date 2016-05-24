/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface UnitProps {
        client : rmx.Game.Client;
        unit   : rmx.Game.WorldObject;
    }

    class UnitSpec extends ReactComponent<UnitProps, {}> {
        render() {
            var unit           = this.props.unit
              , portraitUrl    = rmx.Game.portraitUrl(this.props.client, unit)
              , playerName     = unit.name
              , healthBarLabel = Math.round(unit.healthPercent) + '%'
              , powerBarName   = 'mana'
              , powerBarLabel  = Math.round(unit.powerPercent) + '%'
              , entity         = this.props.client.controlledEntity
              , isSelected     = entity ? entity.targetId == unit.id : false
              , isDead         = unit.health == 0
              , className      = 'unit frame' + (isSelected ? ' selected' : '') + (isDead ? ' dead' : '')
              ;

            return React.DOM.div
                ( { className: className, onClick: this.selectUnit }
                , React.DOM.img({ className: 'portrait', src: portraitUrl })
                , React.DOM.div
                    ( { className: 'bars' }
                    , StatusBar({ className: 'health', text: playerName,   label: healthBarLabel, progress: unit.healthPercent })
                    , StatusBar({ className: 'power',  text: powerBarName, label: powerBarLabel,  progress: unit.powerPercent })
                    )
                );
        }

        selectUnit() {
            rmx.Game.selectEntity(this.props.client, this.props.unit.id);
        }
    }

    export var Unit = createClass(UnitSpec);
}
