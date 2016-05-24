/// <reference path="../Storage/Types.ts" />
/// <reference path="../Pure/Ids.ts" />

// This module provides a generic template description language. It is currently
// only used on the client (to render console messages). The rendering code
// is in the client because it is specific to how the view layer is implemented.
// In our client we use React, so we have
//
//  > function renderTempalte(t: Template): React.Element;

module rmx.Pure.Template {

    // Fragments
    // -------------------------------------------------------------------------

    export class IconF {
        constructor(public ref: rmx.Storage.Reference) {}
    }

    export class EntityF {
        constructor(public entityId: rmx.Pure.EntityId) {}
    }

    export class PlayerF {
        constructor(public accountId: rmx.Pure.AccountId) {}
    }



    export class TemplateFragment {
        constructor
          ( public content : string | IconF | EntityF | PlayerF
            // ^ In the future we may also have: ObjectiveF | PlayerF | PartyF | ...

          , public color : string
            // ^ In CSS hex format (eg. "#0088FF"). This is optional, if not
            // provided then the color is inherited from the parent.
          ) {}
    }

    export type Template = TemplateFragment[];
}
