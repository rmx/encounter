/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />

/// <reference path="./UnitFrame.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface WorldObjectFrameProps {
        client    : rmx.Game.Client;
        unit      : rmx.Game.WorldObject;
        className : string;
    }

    class WorldObjectFrameSpec extends ReactComponent<WorldObjectFrameProps, {}> {
        render() {
            var client = this.props.client
              , unit   = this.props.unit;

            if (unit) {
                return React.DOM.div
                    ( { className: this.props.className }
                    , UnitFrame({ client: client, unit: unit })
                    , React.DOM.div
                        ( { className: 'unit-cast-bar' }
                        , UnitCastBar({ client: client, unit: unit })
                        )
                    , UnitAuras({ client: client, unit: unit })
                    );

            } else {
                return React.DOM.div();
            }
        }
    }

    export var WorldObjectFrame = createClass(WorldObjectFrameSpec);
}
