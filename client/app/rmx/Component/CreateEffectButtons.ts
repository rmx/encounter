/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component {

    export interface CreateEffectButtonsProps {
        effectTypes;
        effectConstructor;
        collection;
    }

    class CreateEffectButtonsSpec extends ReactComponent<CreateEffectButtonsProps, {}> {
        render() {
            var self = this;

            var buttons = this.props.effectTypes.map(x => {
                function create() {
                    self.create(x.type, x.ctor);
                }

                return React.DOM.button
                    ( { className: 'small primary button', onClick: create, key: x.type }
                    , x.type
                    );
            });

            return React.DOM.div
                ( {}
                , React.DOM.div
                    ( { className: 'form' }
                    , React.DOM.label({}, 'add effect')
                    )
                , buttons
                );
        }

        create(type, contentConstructor) {
            var effect = Avers.mk(contentConstructor, {})
              , json   = { type: type, effect: Avers.toJSON(effect) }
              , item   = Avers.mk<any>(this.props.effectConstructor, json);

            rmx.data.pushItem(this.props.collection, item);
        }
    }

    export var CreateEffectButtons = createClass(CreateEffectButtonsSpec);
}
