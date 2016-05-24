/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />


module rmx.Component.View {

    class ErrorSpec extends ReactComponent<{}, {}> {
        render() {
            return Page
                ( {}
                , NavBar
                    ( {}
                    , React.DOM.div({ className: 'title' }, 'Error')
                    )
                , Main
                    ( {}
                    , React.DOM.div
                        ( { className: 'rmx error' }
                        , React.DOM.div
                            ( { className: 'content' }
                            , React.DOM.div
                                ( { className: 'header' }
                                , 'The server is not responding'
                                )
                            , React.DOM.p
                                ( { className: 'reason' }
                                , rmx.error
                                )
                            , React.DOM.button
                                ( { className: 'primary fluid button', onClick: this.reload }
                                , 'reload'
                                )
                            )
                        )
                    )
                );
        }

        reload() {
            window.location.reload();
        }
    }

    export var Error = createClass(ErrorSpec);
}
