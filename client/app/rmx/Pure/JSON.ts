/// <reference path="./String.ts" />

// This file provides functions to deal with JSON en/decoding.

module rmx.Pure.JSON {

    export function
    enumToJSON(en: { [key: number]: string }, value: number): string {
        if (value != null) {
            return dasherize(en[value]).slice(1);
        } else {
            return null;
        }
    }

    export function
    enumFromJSON(en, value: string): number {
        if (value != null) {
            return en[classify(value)];
        } else {
            return null;
        }
    }

    export function
    def(json: any, field: string): any {
        if (json[field] === undefined) {
            throw new Error("Field not present: " + field);
        }

        return json[field];
    }

    export function
    opt(json: any, field: string, parseJSON): any {
        if (json[field] == null) {
            return null;
        } else {
            return parseJSON(json, field);
        }
    }


    function
    ofType(json: any, field: string, expectedType: string): any {
        var value      = def(json, field)
          , actualType = typeof value;

        if (actualType !== expectedType) {
            throw new Error(
                [ 'Expected '
                , field
                , ' to be a '
                , expectedType
                , ', but got a '
                , actualType
                ].join('')
            );
        }

        return value;
    }



    export function
    boolean(json: any, field: string): any {
        return ofType(json, field, 'boolean');
    }

    export function
    string(json: any, field: string): string {
        return ofType(json, field, 'string');
    }

    export function
    number(json: any, field: string): number {
        return ofType(json, field, 'number');
    }

    export function
    object(json: any, field: string): any {
        return ofType(json, field, 'object');
    }
}



module rmx.Pure {

    function variantType(value, map) {
        for (var key in map) {
            if (map[key] === value.constructor) {
                return key;
            }
        }
    }

    export function
    variantToJSON(value, map) {
        var type = variantType(value, map);
        if (type) {
            return { type    : type
                   , content : value.toJSON()
                   };
        } else {
            throw new Error("Unknown value: " + value);
        }
    }

    export function
    variantFromJSON(json, map) {
        var ctor = map[json.type];
        if (ctor) {
            return ctor.parseJSON(json.content);
        } else {
            throw new Error("Unknown type: " + json.type);
        }
    }


    export function
    def(json: any, field: string): any {
        if (json[field] === undefined) {
            throw new Error("Field not present: " + field);
        }

        return json[field];
    }

    export function
    opt(json: any, field: string, parseJSON): any {
        if (json[field] == null) {
            return null;
        } else {
            return parseJSON(json, field);
        }
    }

    export function
    fromMaybe(def, json: any, field: string): any {
        if (json[field] === undefined) {
            return def;
        } else {
            return json[field];
        }
    }


    export function
    arrayFromJSON<T>(json, parseJSON: (x: any) => T): T[] {
        if (json) {
            return json.map(parseJSON);
        } else {
            return null;
        }
    }

    export function
    arrayToJSON(xs) {
        if (xs) {
            return xs.map(x => { return x.toJSON(); });
        } else {
            return null;
        }
    }
}
