module rmx.Views {

    export function
    auraSidebarItems
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resourceId : string
    ): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId + '/resources/' + resourceId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Settings'
                 , description:  'Basic aura settings.'
                 }
               , { href:         contextPath + '/effects'
                 , iconClass:    'icon-cog'
                 , label:        'Effects'
                 , description:  'Permanent effects the aura has on its holder.'
                 }
               , { href:         contextPath + '/event-handlers'
                 , iconClass:    'icon-cog'
                 , label:        'Event handlers'
                 , description:  'How the aura reacts to events.'
                 }
               , { href:         contextPath + '/visual'
                 , iconClass:    'icon-monitor-screen'
                 , label:        'Appearance and sound'
                 , description:  'Particle effects, model animations, sounds.'
                 }
               ];
    }

    export function
    auraMainView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Aura>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.AuraMainView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    auraEffectsView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Aura>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.AuraEffectsView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    auraEventHandlersView
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Aura>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.AuraEventHandlers,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }

    export function
    auraSensousView
    ( encounter  : rmx.data.Object<rmx.Storage.Encounter>
    , resource   : rmx.Storage.Resource<rmx.Storage.Aura>
    ): rmx.View {
        function body(): any {
            var enc =
                rmx.data.findById<rmx.Storage.Encounter>(encounter.objectId).get(null);
            return React.createElement(rmx.Components.AuraSensousView,
                { encounter : enc
                , resource  : resource
                }
            );
        }

        return new rmx.View(body);
    }
}
