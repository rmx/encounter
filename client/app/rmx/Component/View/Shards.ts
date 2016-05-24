/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../CollectionItemHeader.ts" />

/// <reference path="../../data.ts" />


module rmx.Component.View {

    class ShardsSpec extends ReactComponent<{}, {}> {
        render() {
            var children = rmx.data.activeShards.ids.get([]).map(shardId => {
                return rmx.data.findById<rmx.Storage.Shard>(shardId).fmap(shard => {
                    var health = rmx.data.shardHealth(shardId).get(null);
                    var load = health ? health.load : 0;

                    return React.DOM.div
                        ( { key: shardId }
                        , CollectionItemHeader
                            ( { id: shardId, text: shard.content.address.join(':') }
                            )
                        , 'load: '
                        , load
                        );
                }).get(null);
            });

            return Page
                ( {}
                , NavBar({})
                , Main({ className: 'vertical' }, children)
                );
        }
    }

    export var Shards = createClass(ShardsSpec);
}
