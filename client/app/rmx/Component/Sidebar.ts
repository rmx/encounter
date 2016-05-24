/// <reference path="../data.ts" />

/// <reference path="./Base.ts" />
/// <reference path="./Link.ts" />


module rmx.Component {

    export interface SidebarProps {
        items;
    }

    class SidebarSpec extends ReactComponent<SidebarProps, {}> {
        render() {
            var items = this.props.items.map(item => {
                return SidebarItem({ key: item.href, item: item });
            });

            return React.DOM.div({ className: 'sidebar' }, items);
        }
    }

    export var Sidebar = createClass(SidebarSpec);



    export interface SidebarItemProps {
        item;
    }

    class SidebarItemSpec extends ReactComponent<SidebarItemProps, {}> {
        render() {
            var item = this.props.item;

            return Link
                ( { className: 'sidebar-item', href: item.href }
                , React.DOM.i({ className: item.iconClass })
                , React.DOM.div
                    ( { className: 'sidebar-item-content' }
                    , React.DOM.div({ className: 'label' }, item.label)
                    , React.DOM.div({}, item.description)
                    )
                );
        }
    }

    var SidebarItem = createClass(SidebarItemSpec);
}
