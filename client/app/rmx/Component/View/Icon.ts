/// <reference path="../../data.ts" />
/// <reference path="../../picker.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Page.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../LoadingScreen.ts" />


module rmx.Component.View {

    export interface IconProps {
        icon : rmx.data.Object<rmx.Storage.Icon>;
    }

    class IconSpec extends ReactComponent<IconProps, {}> {
        render() {
            var icon    = this.props.icon
              , iconUrl = rmx.blobUrl(icon.content.blobId);

            return Page
                ( {}
                , NavBar
                    ( {}
                    , Link({ className: 'title', href: '/icons' }, 'Icons')
                    , React.DOM.div({ className: 'title' }, icon.content.name)
                    , navbarSpacer
                    // TODO: NavBarUserMenu
                    )
                , Main
                    ( { className: 'vertical' }
                    , React.DOM.div
                        ( { className: 'form' }
                        , React.DOM.label({}, 'name')
                        , Input({ object: icon.content, field: 'name' })
                        , React.DOM.label({}, 'icon')
                        , BlobUploader({ object: icon.content, field: 'blobId' })
                        , React.DOM.img({ src: iconUrl })
                        )
                    )
                );
        }
    }

    export var Icon = createClass(IconSpec);
}
