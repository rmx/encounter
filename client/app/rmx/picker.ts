module rmx.picker {

    export interface SearchResult
        extends Computation<rmx.Storage.Reference[]> {
    }

    export interface SelectSearchResult {
        (ref: rmx.Storage.Reference): void;
    }

    export class State {
        constructor
          ( public originPath   : string
          , public titleLabel   : string
          , public searchResult : SearchResult
          , public onSelect     : SelectSearchResult
          ) {}
    }

    export var state : State;



    // initialize
    // -----------------------------------------------------------------------

    export function
    initialize
    ( originPath   : string
      // ^ Where the user should be redirected to when he dismisses the picker.
      // In most cases you should use 'window.location.pathname'.

    , titleLabel   : string
      // ^ The title should explain what needs to be picked.

    , searchResult : SearchResult
      // ^ A computation which returns a list of references.

    , onSelect     : SelectSearchResult
      // Function which should be called when the user selects one of the
      // references from the search result.

    ): void {
        state = new State(originPath, titleLabel, searchResult, onSelect);
    }


    export function
    selectReference(ref: rmx.Storage.Reference): void {
        if (state) {
            state.onSelect(ref);
        }
    }

    export function
    dismiss(): void {
        if (state) {
            rmx.app.navigateTo(state.originPath);
        }
    }
}
