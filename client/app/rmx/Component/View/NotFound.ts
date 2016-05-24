/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../LoadingScreen.ts" />
/// <reference path="../NavBar.ts" />


module rmx.Component.View {

    export interface NotFoundProps {
        info : string;
    }

    class NotFoundSpec extends ReactComponent<NotFoundProps, {}> {
        render() {
            return Page
                ( {}
                , NavBar
                    ( {}
                    , React.DOM.div({ className: 'title' }, 'Nothing found here')
                    , LoadingScreen({}, this.props.info)
                    )
                );
        }
    }

    export var NotFound = createClass(NotFoundSpec);
}

