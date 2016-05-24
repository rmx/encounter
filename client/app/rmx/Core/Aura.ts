/// <reference path="../Storage/Aura.ts" />

module rmx.Core {

    function flattenTypeMap(typeMap) {
        return Object.keys(typeMap).map(type => {
            return { type: type, ctor: typeMap[type] };
        });
    }

    export function
    auraEffects() {
        return flattenTypeMap(rmx.Storage.auraEffectTypes);
    }

    export function
    spellEffects() {
        return flattenTypeMap(rmx.Storage.spellEffectTypes);
    }
}
