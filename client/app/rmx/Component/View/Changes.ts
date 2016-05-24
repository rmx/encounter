/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../LoadingScreen.ts" />
/// <reference path="../NavBar.ts" />


module rmx.Component.View {

    export interface ChangesProps {
        objectId : string;
    }

    class ChangesSpec extends ReactComponent<ChangesProps, {}> {
        render() {
            var objId = this.props.objectId;

            var content = rmx.data.findById(objId).fmap(obj => {
                var revId   = obj.revisionId
                  , content = [];

                for (var i = revId; i >= 0; --i) {
                    var patch = rmx.data.findPatch(objId, i).fmap(p => {
                        return Patch({ patch: p });
                    }).get(<any>React.DOM.div({}, 'Loading...'));

                    content.push(patch);

                    if (content.length > 10) {
                        break;
                    }
                }

                return React.DOM.div({}, content);

            }).get(React.DOM.div({}, 'Loading...'));


            return Page
                ( {}
                , NavBar
                    ( {}
                    , React.DOM.div({ className: 'title' }, 'Changes')
                    )
                , Main
                    ( { className: 'vertical' }
                    , content
                    )
                );
        }
    }

    export var Changes = createClass(ChangesSpec);



    export interface PatchProps {
        patch : any;
    }

    class PatchSpec extends ReactComponent<PatchProps, {}> {
        render() {
            var patch = this.props.patch;

            return React.DOM.div
                ( { className: 'rmx patch' }
                , React.DOM.div({ className: 'rev' }, patch.revisionId)
                , React.DOM.div
                    ( { className: 'description' }
                    , React.DOM.div
                        ( { className: 'header' }
                        , Time({ datetime: patch.createdAt })
                        , patch.id
                        )
                    , PatchOperation({ op: patch.operation })
                    )
                );
        }
    }

    export var Patch = createClass(PatchSpec);



    export interface PatchOperationProps {
        op : any;
    }

    class PatchOperationSpec extends ReactComponent<PatchOperationProps, {}> {
        render() {
            var op = this.props.op;

            switch (op.type) {
            case 'set':
                return React.DOM.div
                    ( {}
                    , 'Set '
                    , React.DOM.em({}, op.path)
                    , ' to '
                    , React.DOM.em({}, op.value)
                    );

            case 'splice':
                return React.DOM.div
                    ( {}
                    , 'Remove '
                    , op.remove
                    , ' items from '
                    , React.DOM.em({}, op.path)
                    , ' add '
                    , op.insert.length
                    , ' items at position '
                    , op.index
                    );
            }
        }
    }

    export var PatchOperation = createClass(PatchOperationSpec);
}
