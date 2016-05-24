
var __hasProp = {}.hasOwnProperty;
var slice     = Array.prototype.slice;

import { intersperse } from '../Game';
import { _ } from 'underscore';



export function mix(obj: Object, env: Object): void {
    for (var k in env) {
        if (__hasProp.call(obj, k)) {
            throw new Error("Game.Env.mix: Object already contains property '" + k + "'");
        }

        Object.defineProperty(obj, k, Object.getOwnPropertyDescriptor(env, k));
    }
}


export function merge(...envs: Object[]): Object {
    var env = Object.create(null);
    envs.forEach(x => { mix(env, x); });
    return env;
}


export function defineAccessor(obj: Object, name: string, get) {
    Object.defineProperty(obj, name, { enumerable: true, get: get });
}



// Param checks
// -------------------------------------------------------------------------

// True if 'value' is compatible with the given 'type'. The type can be
// given as a class constructor or one of the builtin types (String,
// Number, Function, Boolean).

function isCompatibleType(value, type, typeName: string): boolean {
    if (type === Function) {
        return _.isFunction(value);

    } else if (type === Object) {
        return _.isObject(value);

    } else if (type === Array) {
        return _.isArray(value);

    } else if (type === String || type === Number || type === Boolean) {
        return type === value.constructor;

    } else {
        return typeName === value.constructor.name;
    }
}


export function
checkParam(value, argIndex, type, typeName: string): void {
    if (value == null) {
        throw new TypeError(
            [ "Parameter "
            , argIndex
            , " is not defined"
            ].join('')
        );

    } else if (!isCompatibleType(value, type, typeName)) {
        throw new TypeError(
            [ "Parameter "
            , argIndex
            , " is not a "
            , typeName
            , " (it's a "
            , value.constructor.name
            , ")"
            ].join('')
        );
    }
}


export function
checkParamRange(func, value, name, min, max) {
    if (!(value >= min && value <= max)) {
        throw new TypeError("" + func + ": parameter '" + name + "' is outside of the range [" + min + ", {max}] (it's " + value + ")");
    }
}

export function
prettyPrintFunction(func, args): string {
    var argList = intersperse(slice.call(args), ", ");
    return [ func, "(" ].concat(argList, [")"]).join('');
}

export function
throwTypeError(func, args, message): void {
    throw new TypeError(
        [ prettyPrintFunction(func, args)
        , ": "
        , message
        ].join('')
    );
}

export function
checkParams(func, args, ...types) {
    if (args.length !== types.length) {
        throwTypeError(func, args,
            [ types.length
            , " arguments required, but only "
            , args.length
            , " arguments found"
            ].join('')
        );
    }

    try {
        slice.call(args).forEach(function(arg, i) {
            if (types[i] != null) {
                checkParam(arg, i+1, types[i], types[i].name);
            }
        });

    } catch (e) {
        throwTypeError(func, args, e.message);
    }
}
