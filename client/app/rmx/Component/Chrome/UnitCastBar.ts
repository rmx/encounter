/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../time.ts" />
/// <reference path="../../Game/Entities/WorldObject.ts" />


module rmx.Component.Chrome {

    export interface UnitCastBarProps {
        unit : rmx.Game.WorldObject;
    }

    class UnitCastBarSpec extends ReactComponent<UnitCastBarProps, {}> {
        render() {
            var unit  = this.props.unit
              , spell = unit.spell;

            if (spell) {
                var progress = (rmx.now() - spell.startedAt) / spell.castTime;
                var text = rmx.data.resolveReferenceString<any>(spell.spellId).fmap(s => {
                    return s.content.name;
                }).get('');

                return StatusBar
                    ( { className : 'status cast bar'
                      , text      : text
                      , progress  : progress * 100
                      }
                    );

            } else {
                return StatusBar
                    ( { className : 'invisible cast'
                      , text      : 'x'
                      , progress  : 0
                      }
                    );
            }
        }
    }

    export var UnitCastBar = createClass(UnitCastBarSpec);
}
