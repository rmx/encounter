/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../LoadingScreen.ts" />


module rmx.Component.View {

    export interface HelpArticleProps {
        articleId : string;
    }

    class HelpArticleSpec extends ReactComponent<HelpArticleProps, {}> {
        render() {
            var articleBody = rmx.data.helpArticle(this.props.articleId).fmap(article => {

                var tags = null;
                if (article.attributes.tags && Array.isArray(article.attributes.tags)) {
                    tags = React.DOM.div
                        ( { className: 'tags' }
                        , 'Tags: '
                        , article.attributes.tags
                        );
                }

                return React.DOM.div
                    ( {}
                    , React.DOM.h2
                        ( {}
                        , article.title
                        )
                    , tags
                    , React.DOM.div
                        ( { dangerouslySetInnerHTML: { __html: article.body } }
                        )
                    );
            }).get(<any>LoadingScreen({}, 'Loading...'));

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: '/help' }, 'Help')
                    , React.DOM.div({ className: 'resource' }, this.props.articleId)
                    )
                , Main
                    ( { className: 'vertical' }
                    , React.DOM.div({}, articleBody)
                    )
                );
        }
    }

    export var HelpArticle = createClass(HelpArticleSpec);
}
