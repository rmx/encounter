/// <reference path="../Base.ts" />
/// <reference path="./AuraIcon.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game/Entities/WorldObject.ts" />


module rmx.Component.Chrome {

    export interface UnitAurasProps {
        unit : rmx.Game.WorldObject;
    }

    class UnitAurasSpec extends ReactComponent<UnitAurasProps, {}> {
        render() {
            var auras = this.props.unit.auraList.map(function(aura) {
                return auraIcon(aura);
            });

            return React.DOM.div({ className: 'auras' }, auras);
        }
    }

    export var UnitAuras = createClass(UnitAurasSpec);
}
