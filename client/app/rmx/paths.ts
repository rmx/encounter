/// <reference path="./Storage/Reference.ts" />
//
module rmx.paths {

    // toResourcePath
    // -----------------------------------------------------------------------
    //
    // TODO: Make this generic, dispatch based on what the reference
    // resolves to (top-level object, resource, ...).

    export function
    toResourcePath(ref: rmx.Storage.Reference): string {
        if (ref.path) {
            return '/o/' + ref.objectId + '/' + ref.path.replace('.', '/');
        } else if (ref.objectId) {
            return '/o/' + ref.objectId;
        }
    }
}
