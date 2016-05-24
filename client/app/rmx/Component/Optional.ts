/// <reference path="./Base.ts" />
/// <reference path="./Checkbox.ts" />

module rmx.Component {

    export interface OptionalProps extends ReactProps {
        opt : { enabled: boolean; };
    }

    class OptionalSpec extends ReactComponent<OptionalProps, {}> {

        render() {
            var opt          = this.props.opt
              , contentClass = opt.enabled ? 'content' : 'disabled content';

            return React.DOM.div
                ( { className: 'rmx optional' }
                , React.DOM.div
                    ( { className: 'toggle' }
                    , Checkbox({ object: opt, field: 'enabled' })
                    , 'Enable'
                    )
                , React.DOM.div
                    ( { className: contentClass }
                    , this.props.children
                    )
                );
        }
    }

    export var Optional = createClass(OptionalSpec);
}
