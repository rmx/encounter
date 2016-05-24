/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../Link.ts" />


module rmx.Component.View {

    export interface HelpIndexProps {
        articleId : string;
    }

    class HelpIndexSpec extends ReactComponent<HelpIndexProps, {}> {
        render() {
            var helpArticles = rmx.Help.allArticles.map(helpArticle => {
                return Link
                    ( { href: '/help/' + helpArticle.id, key: helpArticle.id }
                    , helpArticle.id
                    );
            });

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: '/help' }, 'Help')
                    , React.DOM.div({ className: 'resource' }, this.props.articleId)
                    )
                , Main
                    ( { className: 'vertical' }
                    , React.DOM.div({}, helpArticles)
                    )
                );
        }
    }

    export var HelpIndex = createClass(HelpIndexSpec);
}
