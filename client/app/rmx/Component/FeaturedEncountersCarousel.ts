/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />


module rmx.Component.View {

    function toRange(x, l) {
        while (x < 0) {
            x += l;
        }

        return x % l;
    }

    export interface FeaturedEncountersCarouselState {
        index     : number;
        prevIndex : number;
    }

    class FeaturedEncountersCarouselSpec extends ReactComponent<{}, FeaturedEncountersCarouselState> {

        getInitialState() {
            return { index: 0, prevIndex: -1 };
        }

        render() {
            var featuredEncounterIds = rmx.data.featuredEncounters.ids.get([]).map(id => {
                var encounter = rmx.data.objectContent<rmx.Storage.Encounter>(id);

                return encounter.fmap(enc => {
                    if (enc.images.marquee) {
                        return id;
                    } else {
                        return Computation.Pending;
                    }
                }).get(null);

            }).filter(x => {
                return !!x;
            });

            var index     = this.state.index
              , prevIndex = this.state.prevIndex;

            var featuredEncounters = featuredEncounterIds.map((id, idx) => {
                var encounter = rmx.data.objectContent<rmx.Storage.Encounter>(id).get(null);

                var imageUrl = rmx.blobUrl(encounter.images.marquee);
                var tagline  = encounter.tagline;

                var style =
                    { backgroundImage: 'url(' + imageUrl + ')'
                    };

                var encounterStyle: any = {};

                if (idx === toRange(index, featuredEncounterIds.length)) {
                    encounterStyle.opacity = '1';
                    encounterStyle.zIndex = '3';
                    if (index > prevIndex) {
                        encounterStyle.left = '-50%';
                        encounterStyle.transform = 'translateX(50%)';
                    } else {
                        encounterStyle.left = '50%';
                        encounterStyle.transform = 'translateX(-50%)';
                    }

                } else if (idx === toRange(prevIndex, featuredEncounterIds.length)) {
                    encounterStyle.opacity = '.8';
                    encounterStyle.zIndex = '2';
                    if (index > prevIndex) {
                        encounterStyle.left = '0%';
                        encounterStyle.transform = 'translateX(100%)';
                    } else {
                        encounterStyle.left = '0%';
                        encounterStyle.transform = 'translateX(-100%)';
                    }
                } else {
                    encounterStyle.opacity = '.1';
                    encounterStyle.zIndex = '1';
                }

                return React.DOM.div
                    ( { className: 'encounter', style: encounterStyle, key: id }
                    , React.DOM.div({ className: 'image', style: style })
                    , React.DOM.div({ className: 'caption' }, tagline)
                    , React.DOM.a
                        ( { className: 'large primary button', href: '/e/' + id }
                        , 'Select'
                        )
                    );
            }).filter(x => { return !!x; });

            return React.DOM.div
                ( { className: 'rmx featured-encounters-carousel' }
                , React.DOM.div({ className: 'encounters' }, featuredEncounters)
                , React.DOM.div({ className: 'prev carousel-button', onClick: this.next }, React.DOM.i({ className: 'left angle icon' }))
                , React.DOM.div({ className: 'next carousel-button', onClick: this.prev }, React.DOM.i({ className: 'right angle icon' }))
                );
        }

        next() {
            this.setState(
                { index     : this.state.index + 1
                , prevIndex : this.state.index
                }
            );
        }

        prev() {
            this.setState(
                { index     : this.state.index - 1
                , prevIndex : this.state.index
                }
            );
        }
    }

    export var FeaturedEncountersCarousel = createClass(FeaturedEncountersCarouselSpec);
}
