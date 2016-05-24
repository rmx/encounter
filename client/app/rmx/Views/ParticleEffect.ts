module rmx.Views.ParticleEffect {

    function sidebarItems(particleeffect: rmx.data.Object<rmx.Storage.ParticleEffect>): ISidebarItem[] {
        var contextPath = '/o/' + particleeffect.objectId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-cog'
                 , label:        'Definition'
                 , description:  'Particle effect definiton.'
                 }
               , { href:         contextPath + '/effect'
                 , iconClass:    'icon-cog'
                 , label:        'Effect'
                 , description:  'See it in action..'
                 }
               ];
    }

    export function
    mainView(objectId: string) : rmx.View {
        function body() {
            var particleeffect =
                rmx.data.findById<rmx.Storage.ParticleEffect>(objectId).get(null);

            var headerTitle =
                Views.objectHeaderTitle(particleeffect, '/particleeffects',
                        'All particle effects');

            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle })
                , React.createElement(rmx.Components.ParticleEffectMainView, { sidebarItems: sidebarItems(particleeffect), particleeffect: particleeffect.content})
                );
        }

        return new rmx.View(body);
    }

    export function
    particleEffectViewer(objectId: string): rmx.View {
        function body() {
            var particleeffect = rmx.data.findById<any>(objectId).get(null);

            var headerTitle =
                Views.objectHeaderTitle(particleeffect, '/particleeffects',
                        'All particle effects');

            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle })
                , React.createElement(rmx.Components.ParticleEffectViewer, { sidebarItems: sidebarItems(particleeffect), particleeffect: particleeffect.content})
                );
        }

        return new rmx.View(body);
    }
}
