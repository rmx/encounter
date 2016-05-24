/// <reference path="../Base.ts" />
/// <reference path="../TooltipMixin.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />

module rmx.Component.Chrome {

    var fallbackIconUrl = "https://0.gravatar.com/avatar/7f02c799d087a8fa2cc3d4b25bf0d2e3";


    export interface AuraIconProps {
        iconUrl     : string;
        stackCount  : number;
        displayName : string;
        description : string;
    }

    class AuraIconSpec extends ReactComponent<AuraIconProps, {}> {
        render() {
            var stackCount = null;
            if (this.props.stackCount > 0) {
                stackCount = React.DOM.span({ className: 'label' }, this.props.stackCount);
            }

            return React.DOM.div
                ( { className: 'aura' }
                , React.DOM.img({ src: this.props.iconUrl })
                , stackCount
                );
        }

        tooltipContent() {
            return React.DOM.div
                ( {}
                , this.props.displayName
                , React.DOM.p({}, this.props.description)
                );
        }
    }

    AuraIconSpec.prototype.mixins =
        [ TooltipMixin
        , React.addons.PureRenderMixin
        ];


    var AuraIcon = createClass(AuraIconSpec);

    export function
    auraIcon(aura: rmx.Pure.Aura) {
        var obj         = rmx.data.resolveReferenceString<any>(aura.auraId)
          , iconUrl     = obj.fmap(rmx.data.objectIconUrl).get(fallbackIconUrl)
          , displayName = obj.fmap(s => { return s.content.name; }).get('Unknown Aura')
          , description = obj.fmap(s => { return s.content.description; }).get('No description available')
          ;

        var stackCount = null;
        if (aura.stackCount > 0) {
            stackCount = React.DOM.span({ className: 'label' }, aura.stackCount);
        }

        return AuraIcon(
            { key         : aura.slot
            , iconUrl     : iconUrl
            , stackCount  : aura.stackCount
            , displayName : displayName
            , description : description
            }
        );
    }
}
