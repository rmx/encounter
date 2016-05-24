/// <reference path="./rmx/Views.ts" />

/// <reference path="./rmx/Views/Account.ts" />
/// <reference path="./rmx/Views/Aura.ts" />
/// <reference path="./rmx/Views/Class.ts" />
/// <reference path="./rmx/Views/Creature.ts" />
/// <reference path="./rmx/Views/Encounter.ts" />
/// <reference path="./rmx/Views/Model.ts" />
/// <reference path="./rmx/Views/ParticleEffect.ts" />
/// <reference path="./rmx/Views/Skybox.ts" />
/// <reference path="./rmx/Views/Sound.ts" />
/// <reference path="./rmx/Views/Spell.ts" />
/// <reference path="./rmx/Views/Terrain.ts" />
/// <reference path="./rmx/Views/Tile.ts" />

/// <reference path="./rmx/picker.ts" />
/// <reference path="./rmx/data.ts" />


declare var page: any;

interface IPageContext {
    path: string;
    params: any;
}

module Routes {

    // Imports
    // -----------------------------------------------------------------------

    import Aura      = rmx.Storage.Aura;
    import Spell     = rmx.Storage.Spell;



    function defineRoute(path: string, fn: Function): void {
        page(path, function(ctx: IPageContext, next: Function) {
            rmx.app.loadView(function(activateView: Function) {
                fn(activateView, ctx, next);
            });
        });
    }


    function objectRoute
    ( ctor : any
    , path : string
    , fn   : (entity: any, ctx: IPageContext) => rmx.View
    ): void {
        defineRoute('/o/:objectId' + path,
        function(activateView: Function, ctx: IPageContext, next: Function) {
            var objectId = ctx.params.objectId;

            rmx.data.loadById<any>(objectId).promise.then(onLoad);
            function onLoad(entity) {
                if (entity.content instanceof ctor) {
                    activateView(fn(entity, ctx));
                } else {
                    next();
                }
            }
        });
    }


    function asResource<T>(resources, resourceId): rmx.Storage.Resource<T> {
        return Avers.resolvePath<rmx.Storage.Resource<T>>(resources, resourceId);
    }


    function resourceRoute<T>(path: string, fn): void {
        objectRoute(rmx.Storage.Encounter, '/resources/:resourceId' + path,
        function(entity: any, ctx: any): rmx.View {
            var resourceId = ctx.params.resourceId
                , resource   = asResource<T>(entity.content.resources, resourceId)
                , content    = resource.content;

                return fn(entity, resource, content);
        });
    }



    export function setupRoutes(): void {

        defineRoute('*',
        function(activateView: Function, ctx: IPageContext, next: Function) {
            if (rmx.data.session.status === rmx.SessionStatus.Error) {
                activateView(new rmx.View(() => {
                    return rmx.Component.View.Error();
                }));
            } else {
                next();
            }
        });



        defineRoute('/',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Index();
            }));
        });


        defineRoute('/openGames',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.OpenGames();
            }));
        });



        defineRoute('/login',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Login();
            }));
        });

        defineRoute('/signup',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Signup();
            }));
        });



        defineRoute('/e/:encounterId',
        function(activateView: Function, ctx: IPageContext) {
            var encounterId = ctx.params.encounterId;

            rmx.data.loadById<any>(encounterId).promise.then(onLoad);
            function onLoad(entity) {
                if (entity.status === rmx.data.Status.Failed) {
                    activateView(rmx.Views.notFoundView('Encounter ' + encounterId + ' not found'));

                } else {
                    if (entity.content instanceof rmx.Storage.Encounter) {
                        activateView(new rmx.View(() => {
                            return rmx.Component.View.PublicEncounterPage({ encounter: entity });
                        }));

                    } else {
                        activateView(rmx.Views.notFoundView('Encounter ' + encounterId + ' not found'));
                    }
                }
            }
        });

        defineRoute('/e/:encounterId/leaderboard',
        function(activateView: Function, ctx: IPageContext) {
            var encounterId = ctx.params.encounterId;

            rmx.data.loadById<any>(encounterId).promise.then(onLoad);
            function onLoad(entity) {
                if (entity.status === rmx.data.Status.Failed) {
                    activateView(rmx.Views.notFoundView('Encounter ' + encounterId + ' not found'));

                } else {
                    if (entity.content instanceof rmx.Storage.Encounter) {
                        activateView(new rmx.View(() => {
                            return rmx.Component.View.EncounterLeaderboard({ encounterId: encounterId });
                        }));

                    } else {
                        activateView(rmx.Views.notFoundView('Encounter ' + encounterId + ' not found'));
                    }
                }
            }
        });



        defineRoute('*',
        function(activateView: Function, ctx: IPageContext, next: Function) {
            switch (rmx.data.session.status) {
            case rmx.SessionStatus.Unknown:
            case rmx.SessionStatus.Restoring:
                return activateView(new rmx.View(() => {
                    return rmx.Component.LoadingScreen();
                }));

            case rmx.SessionStatus.Authenticated:
                return next();

            default:
                return activateView(new rmx.View(() => {
                    return rmx.Component.View.Login();
                }));
            }
        });


        defineRoute('/call',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Call();
            }));
        });

        defineRoute('/settings',
        function(activateView: Function) {
            rmx.app.navigateTo('/settings/profile');
        });

        defineRoute('/settings/profile',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Components.standardLayout
                    ( rmx.Component.AppNavBar()
                    , React.createElement(rmx.Components.ProfileSettings, { sidebarItems: rmx.Views.settingsSidebarItems() })
                    );
            }));
        });

        defineRoute('/settings/graphics',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.GraphicsSettings();
            }));
        });

        defineRoute('/settings/audio',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.audioSettings();
            }));
        });

        defineRoute('/settings/friends',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                var obj = rmx.data.loadById<rmx.Storage.Account>(<string>rmx.data.session.accountId);
                if (obj.content) {
                    return rmx.Components.standardLayout
                        ( rmx.Component.AppNavBar()
                        , React.createElement(rmx.Components.AccountFriends,
                            { sidebarItems : rmx.Views.settingsSidebarItems()
                            , account      : obj
                            }
                          )
                        );
                } else {
                    return rmx.Component.View.NotFound({ info: 'Loading...' });
                }
            }));
        });

        page('/logout', function(ctx: any, next: Function) {
            rmx.signout(rmx.data.session);
            rmx.app.navigateTo('/login');
        });

        defineRoute('/games',
        function(activateView: Function, ctx: IPageContext) {
            activateView(rmx.Views.collectionView(rmx.data.openGames, 'games', {}, true));
        });

        defineRoute('/recommendations',
        function(activateView: Function, ctx: IPageContext) {
            activateView(rmx.Views.collectionView(rmx.data.recommendations, 'encounters', {}, true));
        });

        defineRoute('/browse/new',
        function(activateView: Function, ctx: IPageContext) {
            activateView(rmx.Views.collectionView(rmx.data.newReleases, 'encounters', {}, true));
        });



        defineRoute('/games/:gameId',
        function(activateView: Function, ctx: IPageContext) {
            var client = rmx.openGame(ctx.params.gameId);

            activateView(new rmx.View(() => {
                if (client.state.stage === rmx.Pure.Stage.Setup) {
                    return rmx.Component.View.GameLobby({ client: client });
                } else {
                    return rmx.Component.View.Game({ client: client });
                }
            }), client.audioHandle);
        });

        defineRoute('/games/:gameId/wardrobe',
        function(activateView: Function, ctx: IPageContext) {
            var client = rmx.openGame(ctx.params.gameId);
            activateView(new rmx.View(() => {
                return rmx.Component.View.GameSetupWardrobe({ client: client });
            }));
        });

        defineRoute('/games/:gameId/log',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.GameLog({ gameId: ctx.params.gameId });
            }));
        });

        defineRoute('/games/:gameId/after',
        function(activateView: Function, ctx: IPageContext) {
            var gameId = ctx.params.gameId;

            activateView(new rmx.View(() => {
                return rmx.data.findById<rmx.Storage.Game>(gameId).fmap(gameE => {
                    var encounterId = gameE.content.encounter.objectId;
                    if (encounterId === rmx.config.tutorialEncounterId) {
                        return rmx.Component.View.GameAfterTutorial({ gameId: gameId });
                    } else {
                        return rmx.Component.View.GameAfter({ gameId: gameId });
                    }
                }).get(React.DOM.div());
            }));
        });

        defineRoute('/survey',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Survey({});
            }));
        });

        defineRoute('/try',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Try({});
            }));
        });


        defineRoute('/accounts',
        function(activateView: Function) {
            var template = rmx.Storage.Account.mk('New Account');
            activateView(rmx.Views.collectionView(rmx.data.accounts, 'accounts', template));
        });

        defineRoute('/shards',
        function(activateView: Function) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.Shards({});
            }));
        });

        defineRoute('/encounters',
        function(activateView: Function) {
            var template = rmx.Storage.Encounter.mk('New Encounter');
            activateView(rmx.Views.collectionView(rmx.data.encounters, 'encounters', template));
        });

        defineRoute('/models',
        function(activateView: Function) {
            var template = rmx.Storage.Model.mk('New Model');
            activateView(rmx.Views.collectionView(rmx.data.models, 'model', template));
        });

        defineRoute('/tiles',
        function(activateView: Function) {
            var template = rmx.Storage.Tile.mk('New Tile');
            activateView(rmx.Views.collectionView(rmx.data.tiles, 'tile', template));
        });

        defineRoute('/skyboxes',
        function(activateView: Function) {
            var template = rmx.Storage.Skybox.mk('New Skybox');
            activateView(rmx.Views.collectionView(rmx.data.skyboxes, 'skybox', template));
        });

        defineRoute('/particleeffects',
        function(activateView: Function) {
            var template = rmx.Storage.ParticleEffect.mk('New Particle Effect');
            activateView(rmx.Views.collectionView(rmx.data.particleeffects, 'particleeffect', template));
        });

        defineRoute('/icons',
        function(activateView: Function) {
            var template = rmx.Storage.Icon.mk('New Icon');
            activateView(rmx.Views.collectionView(rmx.data.icons, 'icon', template));
        });

        defineRoute('/sounds',
        function(activateView: Function) {
            var template = rmx.Storage.Sound.mk('New Sound');
            activateView(rmx.Views.collectionView(rmx.data.sounds, 'sound', template));
        });


        defineRoute('/picker',
        function(activateView: Function) {
            if (rmx.picker.state) {
                activateView(new rmx.View(() => {
                    return rmx.Component.View.Picker({ state: rmx.picker.state });
                }));
            } else {
                rmx.app.navigateTo('/');
            }
        });


        defineRoute('/o/:objectId',
        function(activateView: Function, ctx: IPageContext) {
            var objectId = ctx.params.objectId;

            rmx.data.loadById<any>(objectId).promise.then(onLoad);
            function onLoad(entity) {
                if (entity.status === rmx.data.Status.Failed) {
                    activateView(rmx.Views.notFoundView('Failed to load the object'));

                } else {
                    if (entity.content instanceof rmx.Storage.Encounter) {
                        activateView(rmx.Views.Encounter.mainView(objectId));

                    } else if (entity.content instanceof rmx.Storage.Account) {
                        activateView(rmx.Views.Account.mainView(objectId));

                    } else if (entity.content instanceof rmx.Storage.Model) {
                        var contextPath = ctx.path.replace(/\/$/, '')
                          , object      = entity.content
                          , header      = rmx.Views.objectHeaderTitle(object, '/models', 'All models');

                        activateView(rmx.Views.Model.mainView(contextPath, object, header));

                    } else if (entity.content instanceof rmx.Storage.Tile) {
                        activateView(rmx.Views.Tile.mainView(ctx.path.replace(/\/$/, ''), entity.content));

                    } else if (entity.content instanceof rmx.Storage.Skybox) {
                        activateView(rmx.Views.Skybox.mainView(objectId));

                    } else if (entity.content instanceof rmx.Storage.ParticleEffect) {
                        activateView(rmx.Views.ParticleEffect.mainView(objectId));

                    } else if (entity.content instanceof rmx.Storage.Sound) {
                        activateView(rmx.Views.Sound.mainView(objectId));

                    } else if (entity.content instanceof rmx.Storage.Icon) {
                        activateView(new rmx.View(() => {
                            var icon = rmx.data.findById<rmx.Storage.Icon>(objectId).get(null);
                            return rmx.Component.View.Icon({ icon: icon });
                        }));
                    }
                }
            }
        });

        defineRoute('/o/:objectId/changes',
        function(activateView: Function, ctx: IPageContext) {
            var objectId = ctx.params.objectId;
            activateView(new rmx.View(() => {
                return rmx.Component.View.Changes({ objectId: objectId });
            }));
        });


        // Particle Effects
        // -------------------------------------------------------------------

        objectRoute(rmx.Storage.ParticleEffect, '/effect',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.ParticleEffect.particleEffectViewer(objectId);
        });


        // Account
        // -------------------------------------------------------------------

        objectRoute(rmx.Storage.Account, '/friends',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Account.friendsView(objectId);
        });

        objectRoute(rmx.Storage.Account, '/bindings',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Account.bindingsView(objectId);
        });


        // Encounter
        // -------------------------------------------------------------------

        objectRoute(rmx.Storage.Encounter, '/glue',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Encounter.glueView(objectId);
        });

        objectRoute(rmx.Storage.Encounter, '/scoringFunction',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return new rmx.View(() => {
                return rmx.Component.View.EncounterScoringFunction({ encounterId: objectId });
            });
        });

        objectRoute(rmx.Storage.Encounter, '/images',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Encounter.encounterImagesView(objectId);
        });


        objectRoute(rmx.Storage.Encounter, '/resources',
        function(entity: any, ctx: any) {
            return new rmx.View(() => {
                var objectId = ctx.params.objectId;
                var encounter =
                    rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterResources(
                    { encounter : encounter }
                );
            });
        });

        objectRoute(rmx.Storage.Encounter, '/resources/creatures',
        function(entity: any, ctx: any) {
            return new rmx.View(() => {
                var objectId = ctx.params.objectId;
                var encounter =
                    rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterCreatures(
                    { sidebarItems : rmx.Views.Encounter.mkSidebarItems(entity)
                    , encounter    : encounter
                    }
                );
            });
        });

        objectRoute(rmx.Storage.Encounter, '/resources/spells',
        function(entity: any, ctx: any) {
            return new rmx.View(() => {
                var objectId = ctx.params.objectId;
                var encounter =
                    rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterSpells(
                    { sidebarItems : rmx.Views.Encounter.mkSidebarItems(entity)
                    , encounter    : encounter
                    }
                );
            });
        });

        objectRoute(rmx.Storage.Encounter, '/resources/create',
        function(entity: any, ctx: any) {
            return new rmx.View(() => {
                var objectId = ctx.params.objectId;
                var encounter =
                    rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterResourceCreate(
                    { encounter : encounter }
                );
            });
        });


        objectRoute(rmx.Storage.Encounter, '/parties',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Encounter.encounterPartiesView(objectId);
        });

        objectRoute(rmx.Storage.Encounter, '/terrains',
        function(entity: any, ctx: any) {
            return new rmx.View(() => {
                var objectId = ctx.params.objectId;
                var encounter =
                    rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterTerrains(
                    { encounter : encounter }
                );
            });
        });

        objectRoute(rmx.Storage.Encounter, '/parties/:partyId',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Encounter.encounterPartyView(objectId, ctx.params.partyId);
        });

        objectRoute(rmx.Storage.Encounter, '/games',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return rmx.Views.Encounter.gamesView(objectId);
        });

        objectRoute(rmx.Storage.Encounter, '/issues',
        function(entity: any, ctx: any) {
            var objectId = ctx.params.objectId;
            return new rmx.View(() => {
                var encounter = rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
                return rmx.Component.View.EncounterIssues(
                    { encounter : encounter
                    }
                );
            });
        });

        resourceRoute<any>
        ('', function(entity, resource, content) {
            if (resource.kind == rmx.Storage.Resource.Kind.Inline) {
                if (content instanceof Spell) {
                    return rmx.Views.spellMainView(entity, resource);
                } else if (content instanceof Aura) {
                    return rmx.Views.auraMainView(entity, resource);
                } else if (content instanceof rmx.Storage.Creature) {
                    return rmx.Views.creatureMainView(entity, resource);
                } else if (content instanceof rmx.Storage.Class) {
                    return rmx.Views.Class.mainView(entity, resource.id, resource);
                } else if (content instanceof rmx.Storage.Terrain) {
                    return rmx.Views.terrainMainView(entity, resource);

                } else if (content instanceof rmx.Storage.Behavior) {
                    return new rmx.View(() => {
                        var enc =
                            rmx.data.findById<rmx.Storage.Encounter>(entity.objectId).get(null);
                        return rmx.Component.View.BehaviorResource({ encounter: enc, resource: resource });
                    });

                } else if (content instanceof rmx.Storage.Objective) {
                    return new rmx.View(() => {
                        var enc = rmx.data.findById<rmx.Storage.Encounter>(entity.objectId).get(null);
                        return <any> React.createElement(rmx.Components.ObjectiveResource,
                            { encounter : enc
                            , resource  : resource
                            }
                        );
                    });

                } else {
                    return rmx.Views.notFoundView('No idea how to handle this type of resource');
                }
            } else if (resource.kind == rmx.Storage.Resource.Kind.External) {
                // TODO
            } else if (resource.kind == rmx.Storage.Resource.Kind.Modified) {
                // Render same as inline, but with a note that it was copied
                // from someplace else.
            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });


        resourceRoute<any>
        ('/effects', function(entity, resource, content) {
            if (content instanceof Spell) {
                return new rmx.View(() => {
                    var enc =
                        rmx.data.findById<rmx.Storage.Encounter>(entity.objectId).get(null);
                    return rmx.Component.View.SpellEffects({ encounter: enc, resource: resource });
                });

            } else if (content instanceof Aura) {
                return rmx.Views.auraEffectsView(entity, resource);
            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });

        resourceRoute<any>
        ('/projectile', function(entity, resource, content) {
            if (content instanceof Spell) {
                return new rmx.View(() => {
                    var enc =
                        rmx.data.findById<rmx.Storage.Encounter>(entity.objectId).get(null);
                    return rmx.Component.View.SpellProjectile({ encounter: enc, resource: resource });
                });

            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });

        resourceRoute<any>
        ('/visual', function(entity, resource, content) {
            if (content instanceof rmx.Storage.Spell) {
                return rmx.Views.spellVisualView(entity, resource);
            } else if (content instanceof rmx.Storage.Aura) {
                return rmx.Views.auraSensousView(entity, resource);
            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });

        resourceRoute<any>
        ('/layout', function(entity, resource, content) {
            return rmx.Views.terrainLayoutView(entity, resource);
        });

        resourceRoute<any>
        ('/spells', function(entity, resource, content) {
            if (content instanceof rmx.Storage.Creature) {
                return rmx.Views.creatureSpellsView(entity, resource);
            } else if (content instanceof rmx.Storage.Class) {
                var res = { id: resource.id, type: 'creature', content: content.creature, reference: null, kind: rmx.Storage.Kind.Inline };
                return rmx.Views.creatureSpellsView(entity, res);
            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });

        resourceRoute<any>
        ('/event-handlers', function(entity, resource, content) {
            if (content instanceof Aura) {
                return rmx.Views.auraEventHandlersView(entity, resource);
            } else {
                return rmx.Views.notFoundView('Could not find resource with id ' + resource.id);
            }
        });




        // Model
        // -------------------------------------------------------------------

        objectRoute(rmx.Storage.Model, '/geometry',
        function(entity: any, ctx: any) {
            var contextPath = ctx.path.replace('/geometry', '').replace(/\/$/, '')
              , object      = entity.content
              , header      = rmx.Views.objectHeaderTitle(object, contextPath, 'Back');

            return rmx.Views.Model.geometryView(contextPath, object, header);
        });

        objectRoute(rmx.Storage.Model, '/skins',
        function(entity: any, ctx: any) {
            var contextPath = ctx.path.replace('/skins', '').replace(/\/$/, '')
              , object      = entity.content
              , header      = rmx.Views.objectHeaderTitle(object, contextPath, 'Back');

            return rmx.Views.Model.skinsView(contextPath, object, header);
        });

        objectRoute(rmx.Storage.Model, '/animations',
        function(entity: any, ctx: any) {
            var contextPath = ctx.path.replace('/animations', '').replace(/\/$/, '')
              , object      = entity.content
              , header      = rmx.Views.objectHeaderTitle(object, contextPath, 'Back');

            return rmx.Views.Model.animationsView(contextPath, object, header);
        });



        // Tile
        // -------------------------------------------------------------------

        objectRoute(rmx.Storage.Tile, '/surface',
        function(entity: any, ctx: any) {
            var contextPath = ctx.path.replace('/surface', '').replace(/\/$/, '');
            return rmx.Views.Tile.surfaceView(contextPath, entity.content);
        });

        objectRoute(rmx.Storage.Tile, '/model',
        function(entity: any, ctx: any) {
            var contextPath = ctx.path.replace('/model', '').replace(/\/$/, '')
              , parent      = entity.content
              , object      = parent.model
              , header      = rmx.Views.Tile.modelHeader(contextPath, parent);

            return rmx.Views.Model.mainView(ctx.path.replace(/\/$/, ''), object, header);
        });

        objectRoute(rmx.Storage.Tile, '/model/geometry',
        function(entity: any, ctx: any) {
            var parentPath  = ctx.path.replace('/model/geometry', '').replace(/\/$/, '')
              , contextPath = ctx.path.replace('/geometry', '').replace(/\/$/, '')
              , parent      = entity.content
              , object      = parent.model
              , header      = rmx.Views.Tile.modelHeader(parentPath, parent);

            return rmx.Views.Model.geometryView(contextPath, object, header);
        });

        objectRoute(rmx.Storage.Tile, '/model/skins',
        function(entity: any, ctx: any) {
            var parentPath  = ctx.path.replace('/model/skins', '').replace(/\/$/, '')
              , contextPath = ctx.path.replace('/skins', '').replace(/\/$/, '')
              , parent      = entity.content
              , object      = parent.model
              , header      = rmx.Views.Tile.modelHeader(parentPath, parent);

            return rmx.Views.Model.skinsView(contextPath, object, header);
        });

        objectRoute(rmx.Storage.Tile, '/model/animations',
        function(entity: any, ctx: any) {
            var parentPath  = ctx.path.replace('/model/animations', '').replace(/\/$/, '')
              , contextPath = ctx.path.replace('/animations', '').replace(/\/$/, '')
              , parent      = entity.content
              , object      = parent.model
              , header      = rmx.Views.Tile.modelHeader(parentPath, parent);

            return rmx.Views.Model.animationsView(contextPath, object, header);
        });



        defineRoute('/help',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.HelpIndex();
            }));
        });

        defineRoute('/help/:articleId*',
        function(activateView: Function, ctx: IPageContext) {
            activateView(new rmx.View(() => {
                return rmx.Component.View.HelpArticle(
                    { articleId: ctx.params.articleId }
                );
            }));
        });



        defineRoute('*',
        function(activateView: Function, ctx: IPageContext) {
            activateView(rmx.Views.notFoundView());
        });
    }
}
