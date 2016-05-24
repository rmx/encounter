module rmx.Views.Skybox {

    function headerTitle(skybox) {
        return Views.objectHeaderTitle(skybox, '/skyboxes', 'All skyboxes');
    }

    export function mainView(objectId : string): rmx.View {
        function body() {
            var skybox =
                rmx.data.findById<rmx.Storage.Skybox>(objectId).get(null);
            return rmx.Components.standardLayout
                ( React.createElement(rmx.Components.SiteHeader, { headerTitle: headerTitle(skybox.content) })
                , React.createElement(rmx.Components.SkyboxMainView, { sidebarItems: [], skybox: skybox.content })
                );
        }

        return new rmx.View(body);
    }
}
