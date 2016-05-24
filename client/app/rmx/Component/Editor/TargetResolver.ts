/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />


module rmx.Component.Editor {


    export interface TargetResolverFunctionProps {
        targetResolver : rmx.Storage.TargetResolver;
        fn             : rmx.Storage.TargetResolverFunction;
    }

    class TargetResolverFunctionSpec extends ReactComponent<TargetResolverFunctionProps, {}> {
        render() {
            var fn   = this.props.fn
              , type = Avers.typeName(rmx.Storage.targetResolverFunctionTypes, fn.content.constructor);

            return React.DOM.div
                ( { className: 'target-resolver-function' }
                , React.DOM.div
                    ( { className: 'overlay' }
                    , React.DOM.i({ className: 'icon icon-x', onClick: this.remove })
                    )
                , React.DOM.div({ className: 'name' }, type)
                );
        }

        remove() {
            rmx.data.removeItem(this.props.targetResolver.chain, this.props.fn);
        }
    }

    var TargetResolverFunction = createClass(TargetResolverFunctionSpec);


    export interface TargetResolverProps {
        targetResolver : rmx.Storage.TargetResolver;
    }

    export class TargetResolverSpec extends ReactComponent<TargetResolverProps, {}> {
        render() {
            var targetResolver = this.props.targetResolver;

            var chain: React.ReactElement<any>[] = [
                React.DOM.div({ className: 'spell-target label' }, 'input from user')
            ];

            targetResolver.chain.forEach(function(x) {
                var comp = TargetResolverFunction({ targetResolver: targetResolver, fn: x });
                chain.push(React.DOM.i({ className: "ap icon icon-play" }));
                chain.push(comp);
                chain.push(React.DOM.i({ className: "ap icon icon-play" }));
                chain.push(React.DOM.div({ className: 'spell-target label'}, x.content.outputType));
            });

            var buttons = ['self', 'party', 'party-in-area', 'enemies-in-area'].map(function(x) {
                function addFunction() {
                    var fn = Avers.mk<any>(rmx.Storage.TargetResolverFunction, {
                        type: x, content: {}
                    });
                    rmx.data.pushItem(targetResolver.chain, fn);
                }

                return React.DOM.button( { className: 'small button', onClick: addFunction, key: x }, x);
            });

            var chainArgs = [{ className: 'chain' }].concat(<any>chain);

            return React.DOM.div
                ( { className: 'target-resolver' }
                , React.DOM.div.apply(React.DOM.div, chainArgs)
                , React.DOM.div({}, buttons)
                );
        }
    }

    export var TargetResolver = createClass(TargetResolverSpec);
}
