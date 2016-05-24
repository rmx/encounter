/// <reference path="../../data.ts" />
/// <reference path="../../Components.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../EncounterPageWithSidebar.ts" />
/// <reference path="../ObjectCardCollection.ts" />


module rmx.Component.View {

    export interface EncounterResourceCreateProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
    }

    class EncounterResourceCreateSpec extends ReactComponent<EncounterResourceCreateProps, {}> {
        render() {
            var encounter = this.props.encounter;

            function template(name: string, type: string, klass) {
                var content  = Avers.toJSON(klass.mk('New ' + name))
                  , resource = rmx.Storage.Resource.mkInline(type, content);

                return { name   : name
                       , object : resource
                       };
            }

            var resourceTemplates =
                [ template('Aura'     , 'aura'     , rmx.Storage.Aura)
                , template('Spell'    , 'spell'    , rmx.Storage.Spell)
                , template('Terrain'  , 'terrain'  , rmx.Storage.Terrain)
                , template('Creature' , 'creature' , rmx.Storage.Creature)
                , template('Class'    , 'class'    , rmx.Storage.Class)
                , template('Behavior' , 'behavior' , rmx.Storage.Behavior)
                , template('Objective', 'objective', rmx.Storage.Objective)
                ];

            var buttons = resourceTemplates.map(x => {
                var style = { width: '160px', display: 'inline-block', margin: '0.5em' };

                var self = this;
                function pick() {
                    rmx.data.pushItem
                    ( self.props.encounter.content.resources
                    , x.object
                    );

                    rmx.app.navigateTo(window.location.pathname.replace('create', x.object.id));
                }

                return React.DOM.div
                    ( { style: style, key: x.name }
                    , React.DOM.button
                        ( { className: 'primary button', style: { width: '100%' }, onClick: pick }
                        , x.name
                        )
                    );
            });

            return EncounterPageWithSidebar
                ( { encounter: encounter, vertical: true }
                , React.createElement(rmx.Components.EditorNavbar, { encounter: encounter })
                , React.DOM.div({ style: { display: 'flex', flexWrap: 'wrap', marginTop: '2em' } }, buttons)
                );
        }
    }

    export var EncounterResourceCreate = createClass(EncounterResourceCreateSpec);
}
