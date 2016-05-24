/// <reference path="../../ext/react.d.ts" />
/// <reference path="../../ext/react-addons.d.ts" />

module rmx.Component {

    export interface ReactProps {
        children?: any;
    }

    export class ReactComponent<P, S> {
        mixins : any[];
        props: P;
        state: S;
        refs: { [ref: string]: ReactComponent<any, any>; };
        getDOMNode(): Element { return null; }
        setProps(nextProps: P, callback?: () => void): void {}
        replaceProps(nextProps: P, callback?: () => void): void {}
        setState(nextState: S, callback?: () => void): void {}
        replaceState(nextState: S, callback?: () => void): void {}
        forceUpdate(callback?: () => void): void {}
        isMounted(): boolean { return false; }
    }


    export function
    createClass<P, S>
    ( spec: new() => ReactComponent<P, S>
    ) {
        // Need to delete the constructor (which TypeScript adds), otherwise
        // React will freak out.
        delete spec.prototype.constructor;

        var cls     = React.createClass<P, S>(spec.prototype)
          , factory = React.createFactory<P>(cls);

        (<any>factory)._reactClass = cls;

        return factory;
    }


    export function
    createMixin<P, S>
    ( spec: new() => ReactComponent<P, S>
    ) {
        // Need to delete the constructor (which TypeScript adds), otherwise
        // React will freak out.
        delete spec.prototype.constructor;

        return spec.prototype;
    }
}
