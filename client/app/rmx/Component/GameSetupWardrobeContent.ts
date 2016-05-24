/// <reference path="./Base.ts" />
/// <reference path="./NavBar.ts" />
/// <reference path="./Link.ts" />
/// <reference path="./Body.ts" />
/// <reference path="./Site.ts" />
/// <reference path="./GameSetupBanner.ts" />
/// <reference path="./GameSetupSiteHeader.ts" />
/// <reference path="./GameSetupBody.ts" />
/// <reference path="./GameSetupChat.ts" />
/// <reference path="./RequestAnimationFrameMixin.ts" />

/// <reference path="../data.ts" />
/// <reference path="../Core/Encounter.ts" />
/// <reference path="../Game/Client.ts" />

declare var classNames;

module rmx.Component {

    export interface GameSetupWardrobeContentProps {
        client : rmx.Game.Client;
    }

    export interface GameSetupWardrobeContentState {
        selectedClass: rmx.Storage.Reference;
    }

    class GameSetupWardrobeContentSpec extends ReactComponent<GameSetupWardrobeContentProps, GameSetupWardrobeContentState> {

        getInitialState() {
            return { selectedClass: null };
        }

        render() {
            var client      = this.props.client
              , encounterId = client.state.encounterId;

            if (client.state.stage === rmx.Pure.Stage.Running) {
                rmx.app.navigateTo('/games/' + client.gameId);
            }

            var children = rmx.data.objectContent<rmx.Storage.Encounter>(encounterId).fmap(encounter => {
                var classes = null;

                var currentParty = rmx.Pure.playerParty(client.state, rmx.data.session.accountId);
                encounter.parties.forEach(party => {
                    if (currentParty.id === party.id) {
                        classes = party.classes.map(cls => {
                            // XXX: Ugly.
                            var self = this;

                            function onEnter() {
                                self.setState({ selectedClass: cls });
                            }

                            function onLeave() {
                                self.setState({ selectedClass: null });
                            }

                            return GameSetupClass(
                                { client: client
                                , encounter: encounter
                                , reference: cls
                                , onEnter: onEnter
                                , onLeave: onLeave
                                , key: cls.toString()
                                }
                            );
                        });
                    }
                });

                var player = rmx.Pure.lookupPlayer(client.state, rmx.data.session.accountId);
                var detail = null;
                if (this.state.selectedClass) {
                    detail = ClassDetail({ client: client, encounter: encounter, reference: this.state.selectedClass.toString() });
                } else if (player && player.roleId) {
                    detail = ClassDetail({ client: client, encounter: encounter, reference: player.roleId });
                } else {
                    detail = React.DOM.div
                        ( {}
                        , 'No class selected'
                        );
                }

                return React.DOM.div
                    ( { className: 'children' }
                    , React.DOM.div({ className: 'classes' }, classes)
                    , detail
                    );

            }).get(<any>LoadingScreen({}));

            return React.DOM.div
                ( { className: 'rmx game-setup-wardrobe' }
                , children
                );
        }
    }

    export var GameSetupWardrobeContent = createClass(GameSetupWardrobeContentSpec);



    export interface GameSetupClassProps {
        client    : rmx.Game.Client;
        encounter : rmx.Storage.Encounter;
        reference : rmx.Storage.Reference;

        onEnter   : React.MouseEventHandler;
        onLeave   : React.MouseEventHandler;
    }

    class GameSetupClassSpec extends ReactComponent<GameSetupClassProps, {}> {

        render() {
            // XXX: any
            var cls = rmx.data.resolveReference<any>(this.props.reference)
              , name = cls.fmap(function(x) { return x.content.name; }).get('Loading...')
              ;

            return React.DOM.div
                ( { className: this.elementClass()
                  , onClick: this.handleClick
                  , onMouseEnter: this.props.onEnter
                  , onMouseLeave: this.props.onLeave
                  }
                , React.DOM.div({ className: 'name' }, name)
                // , React.DOM.div({ className: 'rmx wardrobe-class-spells' }, spells)
                );
        }

        elementClass() {
            var player = rmx.Pure.lookupPlayer(this.props.client.state, rmx.data.session.accountId);

            return classNames(
                { rmx              : 1
                , 'wardrobe-class' : 1
                , selected         : player && player.roleId == this.props.reference.toString()
                }
            );
        }

        handleClick() {
            rmx.Game.selectRole(this.props.client, this.props.reference.toString());
        }
    }


    export var GameSetupClass = createClass(GameSetupClassSpec);




    export interface ClassDetailProps {
        client    : rmx.Game.Client;
        encounter : rmx.Storage.Encounter;
        reference : string;
    }

    class ClassDetailSpec extends ReactComponent<ClassDetailProps, {}> {

        render() {
            var res = rmx.data.resolveReferenceString<rmx.Storage.Resource<rmx.Storage.Class>>(this.props.reference);

            return res.fmap(cls => {
                var spellRefs = cls.content.creature.spells;

                var spells = spellRefs.map(spellRef => {
                    var iconId = rmx.data
                        .resolveReference(spellRef)
                        .fmap(rmx.data.objectIconId)
                        .get(null);

                    var spell = rmx.data.resolveReference<rmx.Storage.Resource<rmx.Storage.Spell>>(spellRef);

                    // XXX: any
                    var summary = spell.fmap(res => {
                        return rmx.Core.spellSummary(res.content);
                    }).get(null);

                    var spellName = spell.fmap(res => {
                        return res.content.name;
                    }).get('...');

                    return React.DOM.div
                        ( { className: 'spell', key: spellRef.toString() }
                        , IconImage
                            ( { iconId: iconId }
                            , rmx.data.displayName(spellRef).get('...')
                            , React.DOM.p({}, summary)
                            )
                        , React.DOM.div
                            ( { className: 'details' }
                            , React.DOM.div({ className: 'name' }, spellName)
                            )
                        );
                });

                var modelCanvas = rmx.data.resolveReference<any>(cls.content.creature.model).fmap(m => {
                    return ModelCanvas({ model: m, animationName: 'idle' });
                }).get(null);


                return React.DOM.div
                    ( { className: 'rmx class-details' }
                    , React.DOM.div
                        ( { className: 'content' }

                        , React.DOM.div
                            ( { className: 'sheet' }

                            , React.DOM.div
                                ( { className: 'sheet-content' }

                                , React.DOM.div
                                    ( { className: 'quick-facts' }
                                    , React.DOM.h3({}, 'Quick Facts')
                                    , quickFact('Health', formatRange(cls.content.creature.health))
                                    )

                                , React.DOM.div
                                    ( { className: 'spell-book' }
                                    , React.DOM.h3({}, 'Spells')
                                    , spells
                                    )
                                )
                            )

                        , React.DOM.div
                            ( { className: 'model' }
                            , modelCanvas
                            )
                        )
                    );
            }).get(React.DOM.div());
        }
    }


    export var ClassDetail = createClass(ClassDetailSpec);


    function quickFact(title, value) {
        return React.DOM.div
            ( { className: 'fact' }
            , title
            , ': '
            , value
            );
    }

    function formatRange(range) {
        return '' + range.min + '-' + range.max;
    }
}
