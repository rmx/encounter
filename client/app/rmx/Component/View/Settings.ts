/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../AppNavBar.ts" />


module rmx.Component.View {

    class GraphicsSettingsSpec extends ReactComponent<{}, {}> {
        render() {
            var prefs = rmx.data.preferences
              , g     = prefs.graphics;

            return Page
                ( {}
                , AppNavBar
                    ( {}
                    , React.DOM.div({ className: 'title' }, 'Graphics Settings')
                    )
                , Main
                    ( {}
                    , Sidebar({ items: rmx.Views.settingsSidebarItems() })
                    , React.DOM.div
                        ( { className: 'content' }
                        , React.DOM.div
                            ( { className: 'form' }

                            , React.DOM.h3({}, 'Particle Effects')

                            , React.DOM.label({}, 'enabled')
                            , Checkbox({ object: g.particleEffects, field: 'enabled' })
                            )
                        )
                    )
                );
        }

        reload() {
            window.location.reload();
        }
    }

    export var GraphicsSettings = createClass(GraphicsSettingsSpec);


    export function
    audioSettings() {
        var prefs = rmx.data.preferences
          , a     = prefs.audio;

        return Page
            ( {}
            , AppNavBar
                ( {}
                , React.DOM.div({ className: 'title' }, 'Audio Settings')
                )
            , Main
                ( {}
                , Sidebar({ items: rmx.Views.settingsSidebarItems() })
                , React.DOM.div
                    ( { className: 'content' }
                    , React.DOM.div
                        ( { className: 'form' }

                        , React.DOM.h3({}, 'Audio')

                        , React.DOM.label({}, 'enabled')
                        , Checkbox({ object: a, field: 'enabled' })
                        )
                    )
                )
            );
    }
}
