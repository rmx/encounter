/// <reference path="./Base.ts" />

module rmx.Component {

    export interface StatusBarProps {
        progress   : number;
        text      ?: string;
        label     ?: string;

        className ?: string;
    }

    class StatusBarSpec extends ReactComponent<StatusBarProps, {}> {
        render() {
            var progress  = Math.max(0, Math.min(100, this.props.progress))
              , barStyle  = { width: progress + '%' }
              , className = this.props.className + ' status bar';

            return React.DOM.div
                ( { className: className }
                , React.DOM.div({ className: 'progress', style: barStyle })
                , this.props.text
                , React.DOM.div({ className: 'label' }, this.props.label)
                );
        }
    }

    StatusBarSpec.prototype.mixins =
        [ React.addons.PureRenderMixin
        ];

    export var StatusBar = createClass(StatusBarSpec);
}
