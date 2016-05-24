module rmx.Views.Sound {

    function sidebarItems(sound: rmx.data.Object<rmx.Storage.Sound>): ISidebarItem[] {
        var contextPath = '/o/' + sound.objectId;

        return [ { href: contextPath + '/'
                 , iconClass: 'icon-cog'
                 , label: 'Preview'
                 , description: 'Manage the sound.'
                 }
               ];
    }

    function headerTitle(sound) {
        return Views.objectHeaderTitle(sound, '/sounds', 'All sounds');
    }


    export function mainView(objectId: string): rmx.View {
        function body() {
            var sound =
                rmx.data.findById<rmx.Storage.Sound>(objectId).get(null);
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(sound.content) })
                , React.createElement(rmx.Components.SoundMainView, { sidebarItems : sidebarItems(sound), sound: sound })
                );
        }

        return new rmx.View(body);
    }
}
