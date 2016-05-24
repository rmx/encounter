/// <reference path="../Base.ts" />
/// <reference path="../StatusBar.ts" />
/// <reference path="./Party.ts" />
/// <reference path="./UnitFrame.ts" />

/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    export interface ParentProps {
        client : rmx.Game.Client;
    }

    class ParentSpec extends ReactComponent<ParentProps, {}> {
        render() {
            var client = this.props.client;

            if (client.hideChrome) {
                return React.DOM.div();

            } else {
                var gameFinishedFrame = null;
                if (client.state.stage == rmx.Pure.Stage.Finished)
                    gameFinishedFrame = GameFinishedOverlay({ client: client });

                return React.DOM.div
                    ( { className: 'chrome' }
                    , Party({ client: client })
                    , ActionBar({ client: client })
                    , Console({ client: client })
                    , Objectives({ client: client })
                    , ScrollingCombatLog({ client: client })
                    , centerFrame(client)
                    , gameFinishedFrame
                    );
            }
        }
    }

    export var Parent = createClass(ParentSpec);



    function
    playerCastBar(client: rmx.Game.Client) {
        if (client.controlledEntity && client.controlledEntity.spell) {
            return UnitCastBar({ unit: client.controlledEntity });
        }
    }

    function
    playerAuras(client: rmx.Game.Client) {
        if (client.controlledEntity) {
            return UnitAuras({ client: client, unit: client.controlledEntity });
        }
    }

    function
    targetCastBar(client: rmx.Game.Client) {
        if (client.controlledEntity) {
            var target = rmx.Game.targetEntity(client, client.controlledEntity);
            if (target && target.spell) {
                return UnitCastBar({ unit: target });
            }
        }
    }

    function
    targetAuras(client: rmx.Game.Client) {
        if (client.controlledEntity) {
            var target = rmx.Game.targetEntity(client, client.controlledEntity);
            if (target) {
                return UnitAuras({ client: client, unit: target });
            }
        }
    }

    function
    targetOfTargetFrame(client: rmx.Game.Client) {
        if (client.controlledEntity) {
            var target = rmx.Game.targetEntity(client, client.controlledEntity);
            if (target) {
                var tot = rmx.Game.targetEntity(client, target);
                if (tot) {
                    var style = { width: tot.healthPercent + '%' };
                    return React.DOM.div
                        ( { className: 'target-of-target-frame' }
                        , React.DOM.div({ className: 'bar', style: style })
                        , React.DOM.div
                            ( { className: 'overlay'}
                            , React.DOM.div({ className: 'name' }, tot.name)
                            )
                        );
                }
            }
        }
    }

    function
    centerFrame(client: rmx.Game.Client) {
        var ce = client.controlledEntity;

        return React.DOM.div
            ( { className: 'center-frame' }
            , React.DOM.div
                ( { className: 'target-castbar row' }
                , targetCastBar(client)
                )
            , React.DOM.div
                ( { className: 'player-castbar row' }
                , playerCastBar(client)
                )
            , React.DOM.div
                ( { className: 'unit-frames row' }
                , React.DOM.div
                    ( { className: 'unit-frame-parent' }
                    , ce ? UnitFrame({ client: client, unit: ce }) : undefined
                    , playerAuras(client)
                    )
                , React.DOM.div({ className: 'spacer' })
                , React.DOM.div
                    ( { className: 'target-frame-parent' }
                    , ce ? UnitFrame({ client: client, unit: rmx.Game.targetEntity(client, ce) }) : undefined
                    , targetOfTargetFrame(client)
                    , targetAuras(client)
                    )
                )
            );
    }
}
