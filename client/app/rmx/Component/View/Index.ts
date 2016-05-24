/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../ObjectCardCollection.ts" />
/// <reference path="../FeaturedEncountersCarousel.ts" />
/// <reference path="../../Components.ts" />


module rmx.Component.View {

    function backgroundImageStyle(blobId) {
        if (blobId) {
            return { backgroundImage: 'url(' + rmx.blobUrl(blobId) + ')' };
        } else {
            return {};
        }
    }

    function encounterCard(id, object) {
        return object.fmap(function(x) {
            var href = '/e/' + x.objectId;

            var style = backgroundImageStyle(x.content.images.featureSquare);

            return React.DOM.div
                ( { className: 'rmx encounter-card', key: id }
                , React.DOM.div({ className: 'image', style: style })
                , Link({ href: href, className: 'title' }, x.content.name)
                , React.DOM.div({ className: 'author' }, 'rmx inc.')
                );

        }).get(
            React.DOM.div
                ( { className: 'rmx encounter-card', key: id }
                , React.DOM.div({ className: 'image' })
                , Link({ className: 'title' }, 'Loading...')
                , React.DOM.div({ className: 'author' }, '...')
                )
        );
    }

    class IndexSpec extends ReactComponent<{}, {}> {

        render() {

            var encounters = rmx.data.recommendations.ids.get([]).filter(id => {

                // TODO: Implement filter
                return true;

            }).map(id => {
                var object = rmx.data.findById(id);
                return encounterCard(id, object);
            });

            return Page
                ( {}
                , AppNavBar()
                , Main
                    ( { style: { flexDirection: 'column' } }
                    , FeaturedEncountersCarousel()
                    , ObjectCardCollection
                        ( {}
                        , encounters
                        )
                    )
                );
        }
    }

    export var Index = createClass(IndexSpec);
}
