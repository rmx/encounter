/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../ObjectCard.ts" />
/// <reference path="../ObjectCardCollection.ts" />
/// <reference path="../LoadingScreen.ts" />

module rmx.Component.View {

    export interface PickerProps {
        state;
    }

    class PickerSpec extends ReactComponent<PickerProps, {}> {
        render() {
            var pickerState  = this.props.state
              , filterString = rmx.data.localState.pickerFilter
              , filter       = filterString ? new RegExp(filterString, 'i') : /.*/;

            var items = pickerState.searchResult.fmap(function(refs) {
                var availableItems = refs.filter(function(ref) {
                    return rmx.data.displayName(ref).fmap(function(displayName) {
                        return filter.test(displayName);
                    }).get(false);
                });

                if (availableItems.length > 0) {
                    return availableItems.map(ref => {
                        return PickerResultItem({ state: pickerState, reference: ref, key: ref.toString() });
                    });
                } else {
                    return LoadingScreen({}, 'No items found');
                }
            }).get(React.DOM.div({}, 'Searching...'));

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link
                        ( { className: 'title', href: pickerState.originPath }
                        , pickerState.titleLabel
                        )
                    )
                , React.DOM.div
                    ( { className: 'rmx picker' }
                    , React.DOM.div
                        ( { className: 'header' }
                        , React.DOM.input
                            ( { className: 'fluid rmx input', type: 'text', value: filterString, autoFocus: true, onChange: this.setFilter }
                            )
                        )
                    , Main({}, ObjectCardCollection({}, items))
                    )
                );
        }

        setFilter(ev) {
            rmx.data.localState.pickerFilter = ev.target.value;
        }
    }

    export var Picker = createClass(PickerSpec);



    export interface PickerResultItemProps {
        state;
        reference;
    }

    class PickerResultItemSpec extends ReactComponent<PickerResultItemProps, {}> {

        render() {
            return ObjectCard({ reference: this.props.reference, onSelect: this.onSelect });
        }

        onSelect() {
            this.props.state.onSelect(this.props.reference);
        }
    }

    export var PickerResultItem = createClass(PickerResultItemSpec);
}
