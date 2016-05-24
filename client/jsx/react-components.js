/**
 * @jsx React.DOM
 **/

(function() {

var BlobUploader                     = rmx.Component.BlobUploader._reactClass;
var Body                             = rmx.Component.Body._reactClass;
var Checkbox                         = rmx.Component.Checkbox._reactClass;
var CollectionItemHeader             = rmx.Component.CollectionItemHeader._reactClass;
var CreateEffectButtons              = rmx.Component.CreateEffectButtons._reactClass;
var DrawerCloser                     = rmx.Component.DrawerCloser._reactClass;
var EncounterPageWithSidebar         = rmx.Component.EncounterPageWithSidebar._reactClass;
var EncounterResourcePage            = rmx.Component.EncounterResourcePage._reactClass;
var EncounterResourcePageWithSidebar = rmx.Component.EncounterResourcePageWithSidebar._reactClass;
var Exception                        = rmx.Component.Exception._reactClass;
var FormSectionHeader                = rmx.Component.FormSectionHeader._reactClass;
var IconImage                        = rmx.Component.IconImage._reactClass;
var Image                            = rmx.Component.Image._reactClass;
var Input                            = rmx.Component.Input._reactClass;
var Link                             = rmx.Component.Link._reactClass;
var Main                             = rmx.Component.Main._reactClass;
var MainNav                          = rmx.Component.MainNav._reactClass;
var ModelCanvas                      = rmx.Component.ModelCanvas._reactClass;
var NavBar                           = rmx.Component.NavBar._reactClass;
var NumberInput                      = rmx.Component.NumberInput._reactClass;
var ObjIdPicker                      = rmx.Component.ObjIdPicker._reactClass;
var ObjectCard                       = rmx.Component.ObjectCard._reactClass;
var ObjectCardCollection             = rmx.Component.ObjectCardCollection._reactClass;
var Optional                         = rmx.Component.Optional._reactClass;
var Page                             = rmx.Component.Page._reactClass;
var Reference                        = rmx.Component.Reference._reactClass;
var ReferencePicker                  = rmx.Component.ReferencePicker._reactClass;
var Site                             = rmx.Component.Site._reactClass;
var SiteWithSidebar                  = rmx.Component.SiteWithSidebar._reactClass;
var Text                             = rmx.Component.Text._reactClass;
var VectorInput                      = rmx.Component.VectorInput._reactClass;
var Time                             = rmx.Component.Time._reactClass;

// Editor
var Script                           = rmx.Component.Editor.Script._reactClass;
var Expression                       = rmx.Component.Editor.Expression._reactClass;



rmx.Components.BrowserWarning = React.createClass({
    render: function() {
        return (
            <div className="rmx browser-warning">
                <div className="content">
                    <h1>Your browser is not supported</h1>
                    <p>
                        Because we rely on modern features such as WebGL and
                        Object.observe, rmx
                        only works in modern browsers. Currently supported
                        are:
                    </p>
                    <div>
                        <a href="https://www.google.com/intl/en/chrome/browser/canary.html">Google Chrome Canary</a>
                    </div>
                </div>
            </div>
        );
    }
});


var NavBarSpacer = React.createClass({
    render: function() {
        return (
            <div className="spacer"></div>
        );
    }
});



// DEPRECATED
rmx.Components.SiteHeader = React.createClass({
    render: function() {
        var headerTitle  = this.headerTitle()
          , contextHref  = headerTitle.contextHref
          , contextLabel = headerTitle.contextLabel
          , mainTitle    = headerTitle.mainTitle;

        return (
            <NavBar>
                <Link href={contextHref} className="title">
                    {contextLabel}
                </Link>

                <div className="title">
                    {mainTitle}
                </div>

                <NavBarSpacer />
                <NavBarUserMenu />
            </NavBar>
        );
    },
    headerTitle: function() {
        if (this.props.headerTitle) {
            return this.props.headerTitle;
        } else {
            return { mainTitle    : 'editor'
                   , contextHref  : '/'
                   , contextLabel : 'home'
                   }
        }
    }
});

var CreateItemButtons = React.createClass({
    render: function() {
        var self = this;

        var buttons = this.props.types.map(function(x) {
            function create() {
                self.props.create(x);
            }

            return (
                <button className="small primary button" onClick={create} key={x}>
                    {x}
                </button>
            );
        });

        return (<div>{buttons}</div>);
    }
});

var KillCreature = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , content   = this.props.content;

        var creatures = rmx.Core.resourceSearchResult(encounter, function(res) {
            return res.type === 'creature';
        });

        return (
            <div>
                <label>creature</label>
                <ReferencePicker reference={content.creature} searchResult={creatures} toPath={rmx.paths.toResourcePath}/>

                <label>count</label>
                <NumberInput object={content} field="count" />
            </div>
        );
    }
});

var KillParties = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , content   = this.props.content;

        return (
            <div>
            </div>
        );
    }
});

function taskContent(encounter, content) {
    var props = { encounter: encounter, content: content }
      , type  = Avers.typeName(rmx.Storage.taskTypes, content.constructor);

    switch (type) {
    case 'kill-creature' : return React.createElement(KillCreature, props);
    case 'kill-parties'  : return React.createElement(KillParties, props);
    default              : return <div>Unknown type: {type}</div>;
    }
}

var Task = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , objective = this.props.objective
          , task      = this.props.task
          , type      = Avers.typeName(rmx.Storage.taskTypes, task.content.constructor);

        return (
            <div className="form">
                <CollectionItemHeader id={task.id} text={type} onRemove={this.remove} />
                {taskContent(encounter, task.content)}
            </div>
        );
    },
    remove: function() {
        rmx.data.removeItem(this.props.objective.tasks, this.props.task);
    }
});


rmx.Components.ObjectiveResource = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , objective = this.props.resource.content;

        var tasks = objective.tasks.map(function(task) {
            return <Task encounter={encounter} objective={objective} task={task} key={task.id} />;
        });

        return (
            <EncounterResourcePage encounter={this.props.encounter} resource={this.props.resource} vertical>
                <h1>Objective {this.props.resource.id}</h1>

                <CreateItemButtons
                    types={Object.keys(rmx.Storage.taskTypes)}
                    create={this.createTask} />

                {tasks}
            </EncounterResourcePage>
        );
    },
    createTask: function(type) {
        var ctor    = rmx.Storage.taskTypes[type]
          , content = Avers.mk(ctor, {})
            json    = { type: type, content: Avers.toJSON(content) }
          , task    = Avers.mk(rmx.Storage.Task, json);

        rmx.data.pushItem(this.props.resource.content.tasks, task);
    }
});

rmx.Components.TerrainResource = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , terrain   = resource.content;

        var sidebarItems = rmx.Views.terrainSidebarItems(encounter, resource.id);

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={terrain} field="name" />

                    <label>skybox</label>
                    <ReferencePicker reference={terrain.skybox}
                        searchResult={rmx.data.skyboxes.references} toPath={rmx.paths.toResourcePath} />

                    <label>ambient sound</label>
                    <Reference reference={terrain.sound} />

                    <label>ambient light</label>
                    <Input object={terrain} field="ambientLight" />

                    <label>sun light</label>
                    <Input object={terrain} field="sunLight" />

                    <label>sun position</label>
                    <VectorInput object={terrain} field="sunPos" />

                    <label>shadow darkness</label>
                    <Input object={terrain} field="shadowDarkness" />
                </div>
            </EncounterResourcePageWithSidebar>
        );
    }
});

rmx.Components.EditorNavbar = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , path      = window.location.pathname
          , href1     = '/o/' + encounter.objectId + '/resources/'
          , cls1      = classNames({ item: 1, active: href1 == path })
          , hrefCreatures = '/o/' + encounter.objectId + '/resources/creatures'
          , clsCreatures  = classNames({ item: 1, active: hrefCreatures == path })
          , hrefSpells = '/o/' + encounter.objectId + '/resources/spells'
          , clsSpells  = classNames({ item: 1, active: hrefSpells == path })
          , href2     = '/o/' + encounter.objectId + '/resources/create/'
          , cls2      = classNames({ item: 1, active: href2 == path })
          ;

        return (
            <nav className="ui secondary pointing menu">
                <Link className={cls1} href={href1}>Resources</Link>
                <Link className={clsCreatures} href={hrefCreatures}>Creatures</Link>
                <Link className={clsSpells} href={hrefSpells}>Spells</Link>
                <Link className={cls2} href={href2}>Create</Link>

                <div className="right menu">
                    <div className="item">
                        <div className="ui icon input">
                            <input type="text" placeholder="filter" onChange={this.setFilter} value={rmx.data.localState.resourceFilter} />
                            <i className="search link icon"></i>
                        </div>
                    </div>
                </div>
            </nav>
        );
    },
    setFilter: function(ev) {
        rmx.data.localState.resourceFilter = ev.target.value;
    }
});

rmx.Components.ResourceCard = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , id        = Avers.itemId(encounter.content.resources, resource)
          , href      = '/o/' + encounter.objectId + '/resources/' + id
          , imageUrl  = "http://placehold.it/160x90"
          ;

        if (resource.content && resource.content.iconId) {
            imageUrl = rmx.data.objectIconUrl(resource);
        }

        var name = resource.content ? resource.content.name : '';

        var overlay = <h4>{resource.type}</h4>;
        if (resource.type === 'spell') {
            var description = rmx.Core.spellSummary(resource.content);
            overlay = [
                <h4 key={"h"}>Spell</h4>,
                <p key={"p"}>{description}</p>,
            ];
        } else if (resource.type === 'aura') {
            var description = rmx.Core.auraDescription(resource.content);
            overlay = [
                <h4 key={"h"}>Aura</h4>,
                <p key={"p"}>{description}</p>
            ];
        } else if (resource.type === 'class') {
            var health = resource.content.creature.health;
            overlay = [
                <h4 key={"h"}>Character Class</h4>,
                <p key={"p"}>This class has {resource.content.creature.spells.length} spells in its spellbook and starts with {health.min}-{health.max} health.</p>
            ];
        } else if (resource.type === 'creature') {
            var health = resource.content.health;
            overlay = [
                <h4 key={"h"}>Creature</h4>,
                <p key={"p"}>This creature has {resource.content.spells.length} spells in its spellbook and starts with {health.min}-{health.max} health.</p>
            ];
        }

        return (
            <div className="rmx resource-card">
                <div className="card">
                    <div className="image">
                        <Link href={href}>
                            <img src={imageUrl} />
                        </Link>
                        <Link href={href} className="overlay">
                            {overlay}
                        </Link>
                    </div>
                    <div className="tools">
                        <a onClick={this.remove}>
                            <i className="trash icon"></i>
                        </a>
                    </div>
                </div>
                <div className="display-name">
                    <i className="shield icon"></i>
                    {name || (resource.type + ' ' + resource.id)}
                </div>
            </div>
        );
    },
    remove: function() {
        var index = this.props.encounter.content.resources.indexOf(this.props.resource);
        this.props.encounter.content.resources.splice(index, 1);
    }
});

rmx.Components.standardLayout = function(header, content, footer) {
    return (
        <Body>
            {header}{content}{footer}
        </Body>
    );
}

function backgroundImageStyle(blobId) {
    if (blobId) {
        return { backgroundImage: 'url(' + rmx.blobUrl(blobId) + ')' };
    } else {
        return {};
    }
}

var EncounterCard = React.createClass({
    render: function() {
        var object   = this.props.object
          , toPublicPage = this.props.toPublicPage;

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;
            if (toPublicPage) {
                href = '/e/' + x.objectId;
            }

            var style = backgroundImageStyle(x.content.images.featureSquare);

            return (
                <div className="rmx encounter-card">
                    <div className="image" style={style}></div>
                    <Link href={href} className="title">
                        {x.content.name}
                    </Link>
                    <div className="author">
                        rmx inc.
                    </div>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <div className="image"></div>
                <div className="title">
                    Loading...
                </div>
                <div className="author">
                    ...
                </div>
            </div>
        );
    }
});

var SoundCard = React.createClass({
    render: function() {
        var object = this.props.object
          , imageUrl  = "<* images/sound_icon.svg *>";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;

            return (
                <div className="rmx encounter-card">
                    <img className="image" src={imageUrl} />
                    <Link href={href} className="title">
                        {x.content.name || 'Unnamed Icon'}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <img className="image" src={imageUrl} />
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

var ParticleEffectCard = React.createClass({
    render: function() {
        var object = this.props.object
          , imageUrl  = "<* images/particle_effect_icon.svg *>";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;

            return (
                <div className="rmx encounter-card">
                    <img className="image" src={imageUrl} />
                    <Link href={href} className="title">
                        {x.content.name || 'Unnamed particle effect'}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <img className="image" src={imageUrl} />
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

var IconCard = React.createClass({
    render: function() {
        var object = this.props.object
          , loadingImageUrl  = "http://placehold.it/220x140";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;
            var imageUrl = rmx.blobUrl(x.content.blobId)

            return (
                <div className="rmx encounter-card">
                    <img className="image" src={imageUrl} />
                    <Link href={href} className="title">
                        {x.content.name || 'Unnamed Icon'}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <img className="image" src={loadingImageUrl} />
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

var AccountCard = React.createClass({
    render: function() {
        var objectId = this.props.objectId
          , object   = this.props.object
          , imageUrl = "http://placehold.it/220x140";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;

            return (
                <div className="rmx encounter-card">
                    <div className="image">
                        <img src={imageUrl} />
                    </div>
                    <div>
                        Created: <Time datetime={x.createdAt.toISOString()} />
                    </div>
                    <Link href={href} className="title">
                        {x.content.login || objectId}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <div className="image">
                    <img src={imageUrl} />
                </div>
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

var GameCard = React.createClass({
    render: function() {
        var objectId = this.props.objectId
          , object   = this.props.object
          , imageUrl = "http://placehold.it/220x140";

        var encounterId = rmx.data.findById(objectId).fmap(function(game) {
            return game.content.encounterId;
        }).get();
        var encounterName = rmx.Core.encounterName(encounterId);

        return object.fmap(function(x) {
            var href = '/games/' + x.objectId;

            return (
                <div className="rmx encounter-card">
                    <div className="image">
                        <img src={imageUrl} />
                    </div>
                    <Link href={href} className="title">
                        {objectId}
                    </Link>
                    <div className="author">
                        {encounterName}
                    </div>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <div className="image">
                    <img src={imageUrl} />
                </div>
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

rmx.Components.ModelPreview = React.createClass({
    getInitialState: function() {
        return { animationName: null }
    },
    render: function() {
        var model = this.props.model;

        return (
            <div className="rmx model viewer">
                {ModelCanvas({ model: this.props.model,
                                              animationName: 'idle'})}
            </div>
        );
    }
});

var ModelCard = React.createClass({
    render: function() {
        var object = this.props.object
          , loadingImageUrl  = "http://placehold.it/220x140";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;

            var model;
            if(x.content.model != null)
                model = x.content.model;
            else
                model = x.content;

            return (
                <div className="rmx encounter-card">
                    <div className="image">
                        <img src={loadingImageUrl} />
                    </div>
                    <Link href={href} className="title">
                        {x.content.name || 'Unnamed'}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx encounter-card">
                <div className="image">
                    <img src={loadingImageUrl} />
                </div>
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

var SkyboxCard = React.createClass({
    render: function() {
        var object = this.props.object
          , loadingImageUrl  = "http://placehold.it/55x55";

        return object.fmap(function(x) {
            var href = '/o/' + x.objectId;

            var pz = "http://placehold.it/55x55"
              , px = "http://placehold.it/55x55"
              , nz = "http://placehold.it/55x55"
              , nx = "http://placehold.it/55x55";

            if(x.content.getTexture('pz') != null)
                pz = rmx.blobUrl(x.content.getTexture('pz').blobId);
            if(x.content.getTexture('px') != null)
                px = rmx.blobUrl(x.content.getTexture('px').blobId);
            if(x.content.getTexture('nz') != null)
                nz = rmx.blobUrl(x.content.getTexture('nz').blobId);
            if(x.content.getTexture('nx') != null)
                nx = rmx.blobUrl(x.content.getTexture('nx').blobId);

            return (
                <div className="rmx skybox-card">
                    <div className="image">
                        <img src={pz} />
                        <img src={px} />
                        <img src={nz} />
                        <img src={nx} />
                    </div>
                    <Link href={href} className="title">
                        {x.content.name || 'Unnamed Skybox'}
                    </Link>
                </div>
            );
        }).get(
            <div className="rmx skybox-card">
                <div className="image">
                    <img src={loadingImageUrl} />
                    <img src={loadingImageUrl} />
                    <img src={loadingImageUrl} />
                    <img src={loadingImageUrl} />
                </div>
                <div className="title">
                    Loading...
                </div>
            </div>
        );
    }
});

rmx.Components.Collection = React.createClass({
    render: function() {
        var type   = this.props.type
          , filterString = rmx.data.localState.collectionFilter
          , filter = filterString ? new RegExp(filterString, 'i') : /.*/;

        var toPublicPage = this.props.toPublicPage;

        var entities = this.props.collection.ids.get([]).filter(function(x) {
            var name = rmx.data.findById(x).fmap(function(x) { return x.content.name }).get();
            if (name) {
                return filter.test(name);
            } else {
                return true;
            }
        }).map(function(x) {
            function loaded(entity) {
                var href = '/o/' + x;

                return (
                    <div className="entity" key={x}>
                        <h3>
                            <Link href={href}>
                                {entity.content.name || x}
                            </Link>
                        </h3>
                    </div>
                );
            }

            function placeholder() {
                return (
                    <div className="entity">
                        <h3>
                            Loading...
                        </h3>
                    </div>
                );
            }

            var object = rmx.data.findById(x);
            if (type == 'encounters') {
                return React.createElement(EncounterCard, { key: x, object: object, toPublicPage: toPublicPage });
            } else if (type == 'icon') {
                return React.createElement(IconCard, { key: x, object: object });
            } else if (type == 'tile') {
                return React.createElement(ModelCard, { key: x, object: object });
            } else if (type == 'model') {
                return React.createElement(ModelCard, { key: x, object: object });
            } else if (type == 'skybox') {
                return React.createElement(SkyboxCard, { key: x, object: object });
            } else if (type == 'sound') {
                return React.createElement(SoundCard, { key: x, object: object });
            } else if (type == 'particleeffect') {
                return React.createElement(ParticleEffectCard, { key: x, object: object });
            } else if (type == 'accounts') {
                return React.createElement(AccountCard, { key: x, objectId: x, object: object });
            } else if (type == 'games') {
                return React.createElement(GameCard, { key: x, objectId: x, object: object });
            } else {
                return object.fmap(loaded).get(placeholder());
            }
        });

        return (
            <Body>
                <Site>
                    <MainNav />
                    <DrawerCloser />

                    <NavBar>
                        <div className="title">
                            {type}
                        </div>

                        <div className="filter">
                            <input className="fluid rmx input" type="text" placeholder="filter" onChange={this.setFilter} autoFocus value={filterString} />
                        </div>

                        <div className="rmx primary button" onClick={this.createItem}>
                            Create
                        </div>

                        <NavBarSpacer />

                        <NavBarUserMenu />
                    </NavBar>

                    <main className="main" style={{ flexDirection: 'column' }}>
                        <ObjectCardCollection>
                            {entities}
                        </ObjectCardCollection>
                    </main>
                </Site>
            </Body>
        );
    },
    setFilter: function(ev) {
        rmx.data.localState.collectionFilter = ev.target.value;
    },
    createItem: function(ev) {
        this.props.collection.create(Avers.toJSON(this.props.template));
    }
});


var NavBarUserMenu = React.createClass({
    render: function() {
        if (rmx.data.session.accountId) {
            return React.DOM.div
                ( {}
                , <a className="link" href={"/o/" + rmx.data.session.accountId}>
                    Your Profile
                </a>
                , <span className="button" onClick={this.signout}>
                    Signout
                </span>
                );
        } else {
            return (
                <div className="button" href="/signup">
                    Sign up
                </div>
            );
        }
    },
    signout: function() {
        rmx.signout(rmx.data.session);
    }
});

rmx.Components.Encounter = React.createClass({
    render: function() {
        var encounter = this.props.encounter;

        return (
            <EncounterPageWithSidebar encounter={encounter}>
                <div className="vertical form">
                    <label>name</label>
                    <Input object={encounter.content} field="name" />

                    <label>tagline</label>
                    <Input object={encounter.content} field="tagline" />

                    <label>description</label>
                    <Text object={encounter.content} field="description" />
                </div>
            </EncounterPageWithSidebar>
        );
    }
});

rmx.Components.EncounterGlue = React.createClass({
    render: function() {
        var encounter = this.props.encounter;

        return (
            <EncounterPageWithSidebar encounter={encounter}>
                <div className="vertical form">
                    <label>glue</label>
                    <Script object={encounter.content} field="glue" />
                </div>
            </EncounterPageWithSidebar>
        );
    }
});


var EncounterClass = React.createClass({
    render: function() {
        var self = this;

        var ref         = this.props.reference
          , displayName = rmx.data.displayName(ref).get(ref.toString());

        var obj = rmx.data.resolveReference(ref);
        return obj.fmap(function(x) {
            var nrSpells = x.content.creature.spells.length
              , hp = x.content.creature.health.max;

            var spells = x.content.creature.spells.map(function(spellRef) {
                var iconId = rmx.data
                    .resolveReference(spellRef)
                    .fmap(rmx.data.objectIconId)
                    .get();

                var summary = rmx.data.resolveReference(spellRef).fmap(function(res) {
                    return rmx.Core.spellSummary(res.content);
                }).get();

                var resourceId = spellRef.path.replace('resources.', '');
                var href = '/o/' + spellRef.objectId + '/resources/' + resourceId;

                return (
                    <IconImage iconId={iconId} onClick={rmx.app.navigateToFn(href)} key={spellRef.toString()}>
                        {rmx.data.displayName(spellRef).get('...')}
                        <p>{summary}</p>
                    </IconImage>
                );
            });

            function onSelect() {
                rmx.app.navigateTo(rmx.paths.toResourcePath(ref));
            }

            return (
                <div className="rmx encounter-class-reference">
                    <CollectionItemHeader id={ref.id} text={displayName} onRemove={self.remove} onSelect={onSelect} />
                    <div>
                        This creature starts with {hp} HP and has {nrSpells} spells:
                    </div>
                    <div className="rmx icon-image-list">
                        {spells}
                    </div>
                </div>
            );
        }).get(
            <div>
                <CollectionItemHeader id={ref.id} text={displayName} onRemove={this.remove} />
                <div>
                    This createure starts with 0 HP and has 0 spells.
                </div>
            </div>
        );
    },
    remove: function() {
        rmx.data.removeItem(this.props.classes, this.props.reference);
    }
});

rmx.Components.EncounterImagesView = React.createClass({
    render: function() {
        var encounter = this.props.encounter;

        return (
            <EncounterPageWithSidebar encounter={encounter} vertical>
                <div>
                    <h1>Feature Square</h1>
                    <p>
                        This should be a 200x180px image. It is used on the encounter
                        index page.
                    </p>
                    <BlobUploader object={encounter.content.images} field='featureSquare' />
                    <Image blobId={encounter.content.images.featureSquare} />

                    <h1>Marquee</h1>
                    <p>
                        It should be at least 1600x720px.
                    </p>
                    <BlobUploader object={encounter.content.images} field='marquee' />
                    <Image blobId={encounter.content.images.marquee} />

                    <h1>Background</h1>
                    <p>
                        This image is used for the background on the public encounter page.
                        It should be at least 1600x1200px.
                    </p>
                    <BlobUploader object={encounter.content.images} field='background' />
                    <Image blobId={encounter.content.images.background} />
                </div>
            </EncounterPageWithSidebar>
        );
    }
});

rmx.Components.EncounterParty = React.createClass({
    render: function() {
        var encounter   = this.props.encounter
          , party       = this.props.party;

        var iconUrl = 'http://wow.zamimg.com/images/wow/icons/medium/inv_sword_04.jpg';

        var classes = party.classes.map(function(x, i) {
            return <EncounterClass encounter={encounter} classes={party.classes} reference={x} key={i} />;
        });

        var objectives = party.objectives.map(function(x) {
            var resourceId = x.path.replace('resources.', '')
              , href       = '/o/' + encounter.objectId + '/resources/' + resourceId;

            function remove() {
                rmx.data.removeItem(party.objectives, x);
            }

            function onSelect() {
                rmx.app.navigateTo(href);
            }

            return (
                <div className="rmx encounter-class-reference" key={x.id}>
                    <CollectionItemHeader id={x.id} text={x.path} onRemove={remove} onSelect={onSelect} />
                </div>
            );
        });

        var partyHref = '/o/' + encounter.objectId + '/parties/' + party.id;
        return (
            <EncounterPageWithSidebar encounter={encounter} vertical>
                <div className="form">
                    <h1>Party {party.id}</h1>
                    <Link href={'/o/' + encounter.objectId + '/parties'}>Back</Link>

                    <FormSectionHeader header="Spawn Point" />
                    {rmx.Component.SpawnPoint({ encounter: encounter, spawnPoint: party.spawnPoint })}

                    <FormSectionHeader header="Classes" />
                    <div className="primary fluid button" onClick={this.showClassPicker}>
                        Add a new class
                    </div>
                    {classes}

                    <FormSectionHeader header="Objectives" />
                    <div className="primary fluid button" onClick={this.showObjectivePicker}>
                        Add a new objective
                    </div>
                    {objectives}
                </div>
            </EncounterPageWithSidebar>
        );
    },
    showClassPicker: function() {
        rmx.picker.initialize
          ( window.location.pathname
          , 'Back to encounter ' + this.props.encounter.content.name
          , rmx.Core.resourceSearchResult(this.props.encounter, this.filterClassResource)
          , this.addClass
          );

        rmx.app.navigateTo('/picker');
    },
    filterClassResource: function(res) {
        var isSelected = this.props.party.classes.some(function(x) {
            return x.path === 'resources.' + res.id;
        });

        return res.type === 'class' && !isSelected;
    },
    addClass: function(ref) {
        rmx.data.pushItem(this.props.party.classes, ref);
        rmx.picker.dismiss();
    },
    remove: function() {
        rmx.data.removeItem
          ( this.props.encounter.content.parties
          , this.props.party
          );
    },
    showObjectivePicker: function() {
        rmx.picker.initialize
          ( window.location.pathname
          , 'Back to encounter ' + this.props.encounter.content.name
          , rmx.Core.resourceSearchResult(this.props.encounter, this.filterObjectiveResource)
          , this.addObjective
          );

        rmx.app.navigateTo('/picker');
    },
    filterObjectiveResource: function(res) {
        return res.type === 'objective';
    },
    addObjective: function(ref) {
        rmx.data.pushItem(this.props.party.objectives, ref);
        rmx.picker.dismiss();
    }
});

rmx.Components.EncounterParties = React.createClass({
    render: function() {
        var encounter = this.props.encounter;

        var parties = encounter.content.parties.map(function(x) {
            var href = '/o/' + encounter.objectId + '/parties/' + x.id;

            function remove() {
                rmx.data.removeItem(encounter.content.parties, x);
            }

            function onSelect() {
                rmx.app.navigateTo(href);
            }

            return (
                <div className="rmx encounter-class-reference" key={x.id}>
                    <CollectionItemHeader id={x.id} text={'party'} onRemove={remove} onSelect={onSelect} />
                    <div>
                        Players can choose from {x.classes.length} classes and need to complete {x.objectives.length} objectives. <Link href={href}>open...</Link>
                    </div>
                </div>
            );
        });

        return (
            <EncounterPageWithSidebar encounter={encounter} vertical>
                <div>
                    <div className="primary fluid button" onClick={this.createParty}>
                        Add a new party
                    </div>
                </div>

                <div>
                    {parties}
                </div>
            </EncounterPageWithSidebar>
        );
    },
    createParty: function() {
        rmx.data.pushItem
          ( this.props.encounter.content.parties
          , Avers.mk(rmx.Storage.Party, {})
          );
    }
});

rmx.Components.EncounterGames = React.createClass({
    render: function() {
        var encounter  = this.props.encounter;
        var collection = rmx.data.gamesUsingEncounter.get(encounter.objectId);

        var games = collection.ids.get([]).map(function(id) {
            var imagesCollection = rmx.data.imagesRelatedTo.get(id);
            var images = imagesCollection.ids.get([]).map(function(imgId) {
                var url = rmx.data.findById(imgId).fmap(function(image) {
                    return rmx.blobUrl(image.content.blobId);
                }).get('');

                var style = { width: '100px', height: '100px' };
                return <img src={url} style={style} key={imgId} />;
            });

            return (
                <div key={id}>
                    <CollectionItemHeader id={id} text={id} onRemove={function(){}} />
                    {images}
                </div>
            );
        });

        return (
            <EncounterPageWithSidebar encounter={encounter} vertical>
                <div>
                    {games}
                </div>
            </EncounterPageWithSidebar>
        );
    }
});

rmx.Components.SpellMainView = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , spell     = resource.content;

        var sidebarItems = rmx.Views.spellSidebarItems(encounter, resource.id);

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={spell} field="name" />

                    <label>icon</label>
                    <ObjIdPicker object={spell} field="iconId" searchResult={rmx.data.icons.references} />

                    <label>description</label>
                    <Text object={spell} field="description" />

                    <FormSectionHeader header="Target Type" />
                    <select value={spell.targetType} onChange={this.changeTargetType}>
                        <option value="self">Self</option>
                        <option value="world-object">World Object</option>
                        <option value="location">Location</option>
                    </select>

                    <FormSectionHeader header="range" />
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>min</label>
                            <NumberInput object={spell.range} field="min" />
                        </div>
                        <div className="form-row-element">
                            <label>max</label>
                            <NumberInput object={spell.range} field="max" />
                        </div>
                    </div>

                    <FormSectionHeader header="Cast Time" />
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>min</label>
                            <NumberInput object={spell.castTime} field="min" />
                        </div>
                        <div className="form-row-element">
                            <label>base</label>
                            <NumberInput object={spell.castTime} field="base" />
                        </div>
                    </div>

                    <FormSectionHeader header="Radius" />
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>min</label>
                            <NumberInput object={spell.radius} field="min" />
                        </div>
                        <div className="form-row-element">
                            <label>base</label>
                            <NumberInput object={spell.radius} field="base" />
                        </div>
                        <div className="form-row-element">
                            <label>max</label>
                            <NumberInput object={spell.radius} field="max" />
                        </div>
                    </div>

                    <FormSectionHeader header="Power Cost" />
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>power type</label>
                            <Input object={spell.powerCost} field="powerType" />
                        </div>
                        <div className="form-row-element">
                            <label>amount</label>
                            <NumberInput object={spell.powerCost} field="amount" />
                        </div>
                    </div>

                    <FormSectionHeader header="Cooldown" />
                    <p>
                        The spell cooldown is triggered when the player starts casting
                        the spell. While the cooldown is active, this spell can not be
                        used.
                    </p>
                    <Optional opt={spell.cooldown}>
                        <div className="form-row">
                            <div className="form-row-element">
                                <label>min</label>
                                <NumberInput object={spell.cooldown.content} field="min" />
                            </div>
                            <div className="form-row-element">
                                <label>base</label>
                                <NumberInput object={spell.cooldown.content} field="base" />
                            </div>
                            <div className="form-row-element">
                                <label>max</label>
                                <NumberInput object={spell.cooldown.content} field="max" />
                            </div>
                        </div>
                    </Optional>

                    <FormSectionHeader header="pulse timer" />
                    <p>
                        A channelled spell has a fixed interval, or ticks a given number of times (counter). You can specify sensous effects which will be applied at each tick.
                    </p>
                    <Optional opt={spell.pulseTimer}>
                        <TickTimer tickTimer={spell.pulseTimer.content} />
                    </Optional>
                </div>
            </EncounterResourcePageWithSidebar>
        );
    },
    changeTargetType: function(ev) {
        this.props.resource.content.targetType = ev.target.value;
    }
});

rmx.Components.SpellEffectState = React.createClass({
    render: function() {
        var Reference = rmx.Components.Reference
          , effect = this.props.effect;

        return (
            <div className="form-row">
                <div className="form-row-element">
                    <label>visual</label>
                    <ObjIdPicker object={effect} field="particleId" searchResult={rmx.data.particleeffects.references} />
                </div>
                <div className="form-row-element">
                    <label>sound</label>
                    <ObjIdPicker object={effect} field="soundId" searchResult={rmx.data.sounds.references}/>
                </div>
                <div className="form-row-element">
                    <label>animation</label>
                    <select value={effect.animationId} onChange={this.changeType}>
                        <option value=''>none</option>
                        <option value='attack'>attack</option>
                        <option value='slash'>slash</option>
                        <option value='stab'>stab</option>
                        <option value='slam'>slam</option>
                        <option value='cast'>cast</option>
                        <option value='directed'>directed</option>
                        <option value='remote'>remote</option>
                        <option value='centered'>centered</option>
                        <option value='summon'>summon</option>
                        <option value='block'>block</option>
                    </select>
                </div>
            </div>
        );
    },
    changeType: function(ev) {
        this.props.effect.animationId = ev.target.value;
    }
});

rmx.Components.SpellVisualView = React.createClass({
    render: function() {
        var encounter   = this.props.encounter
          , resource    = this.props.resource
          , spell       = resource.content
          , metaEffects = spell.sensousEffects;

        var sidebarItems = rmx.Views.spellSidebarItems(encounter, resource.id);

        var SpellEffectState = rmx.Components.SpellEffectState;

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}
                vertical>
                <div className="form spell">
                    <label>Cast begin</label>
                    <SpellEffectState effect={metaEffects.castBegin} />
                </div>

                <div className="form spell">
                    <label>Casting</label>
                    <SpellEffectState effect={metaEffects.casting} />
                </div>

                <div className="form spell">
                    <label>Casting pulse</label>
                    <SpellEffectState effect={metaEffects.castingPulse} />
                </div>

                <div className="form spell">
                    <label>Cast end</label>
                    <SpellEffectState effect={metaEffects.castEnd} />
                </div>

                <div className="form spell">
                    <label>Projectile</label>
                    <SpellEffectState effect={metaEffects.projectile} />
                </div>

                <div className="form spell">
                    <label>Spell hit</label>
                    <SpellEffectState effect={metaEffects.spellHit} />
                </div>
            </EncounterResourcePageWithSidebar>
        );
    }
});

rmx.Components.ClassView = React.createClass({
    render: function() {
        var encounter = this.props.encounter;
        var resource = this.props.resource
          , cls      = resource.content;

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={this.props.sidebarItems}>

                <div className="form">
                    <label>name</label>
                    <Input object={cls} field="name" />

                    <label>model</label>
                    <ReferencePicker reference={cls.creature.model}
                        searchResult={rmx.data.models.references} toPath={rmx.paths.toResourcePath} />

                    <label>skin</label>
                    <Input object={cls.creature} field="skinId" />

                    <label>health</label>
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>min</label>
                            <NumberInput object={cls.creature.health} field="min" />
                        </div>
                        <div className="form-row-element">
                            <label>max</label>
                            <NumberInput object={cls.creature.health} field="max" />
                        </div>
                    </div>
                </div>
            </EncounterResourcePageWithSidebar>
        );
    }
});

var CreatureSpellReference = React.createClass({
    render: function() {
        var collection  = this.props.collection
          , item        = this.props.item
          , displayName = rmx.data.displayName(item).get(item.toString())
          , summary     = rmx.data.resolveReference(item).fmap(this.toSummary).get('...');

        function onSelect() {
            rmx.app.navigateTo(rmx.paths.toResourcePath(item));
        }

        return (
            <div className="rmx creature-spell-reference">
                <CollectionItemHeader id={item.id} text={displayName} onRemove={this.remove} onSelect={onSelect} />
                <div>{summary}</div>
            </div>
        );
    },
    toSummary: function(resource) {
        return rmx.Core.spellSummary(resource.content);
    },
    remove: function() {
        rmx.data.removeItem(this.props.collection, this.props.item);
    }
});

rmx.Components.CreatureSpellsView = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , creature  = resource.content;

        var spells = creature.spells.map(function(spell) {
            return <CreatureSpellReference collection={creature.spells} item={spell} key={spell.toString()} />;
        });

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={rmx.Views.creatureSidebarItems(encounter, resource.id)}
                vertical>
                <div className="form">
                    <div className="primary button" onClick={this.showPicker}>
                        Add spell
                    </div>
                    {spells}
                </div>
            </EncounterResourcePageWithSidebar>
        );
    },
    showPicker: function() {
        rmx.picker.initialize
          ( window.location.pathname
          , 'Back to creature ' + this.props.resource.content.name
          , rmx.Core.resourceSearchResult(this.props.encounter, this.filterResource)
          , this.addSpell
          );

        rmx.app.navigateTo('/picker');
    },
    filterResource: function(res) {
        var isSelected = this.props.resource.content.spells.some(function(x) {
            return x.path === 'resources.' + res.id;
        });

        return res.type === 'spell' && !isSelected;
    },
    addSpell: function(ref) {
        rmx.data.pushItem(this.props.resource.content.spells, ref);
        rmx.picker.dismiss();
    }
});

rmx.Components.Account = React.createClass({
    render: function() {
        var account = this.props.account;

        var accountSecret;
        if (account.objectId == rmx.data.session.accountId) {
            accountSecret = <rmx.Components.AccountSecret account={account} />
        }

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <label>login</label>
                    <Input object={account.content} field="login" />

                    <label>email</label>
                    <Input object={account.content} field="email" />
                </div>

                {accountSecret}
            </SiteWithSidebar>
        );
    }
});

rmx.Components.AccountSecret = React.createClass({
    getInitialState: function() {
        return { password: '', confirmation: '', notification: '' };
    },
    render: function() {
        var account   = this.props.account
          , state     = this.state
          , canCommit = state.password.length > 0 && state.password == state.confirmation;

        var notification;
        if (state.notification) {
            notification = <p>{state.notification}</p>
        }

        return (
            <div className="form">
                <h1 className="header">Password</h1>

                <label>password</label>
                <input type="password" value={state.password} onChange={this.changePassword} />

                <label>confirmation</label>
                <input type="password" value={state.confirmation} onChange={this.changeConfirmation} />

                <button className="primary button" disabled={!canCommit} onClick={this.commitChange}>
                    Save
                </button>

                {notification}
            </div>
        );
    },
    changePassword: function(event) {
        this.setState({ notification: '', password: event.target.value });
    },
    changeConfirmation: function(event) {
        this.setState({ notification: '', confirmation: event.target.value });
    },
    commitChange: function() {
        rmx.data.changeSecret(this.state.password).then(function() {
            this.setState({ password: '', confirmation: '', notification: 'success' });
        }.bind(this));
    }
});

rmx.Components.AccountFriends = React.createClass({
    render: function() {
        var account = this.props.account
          , friends = account.content.friends.map(function(id) {
                return <AccountFriend id={id} account={account} key={id} />;
            });

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div>
                    <input type="text" ref="accountId" />
                    <button className="primary button" onClick={this.addFriend}>
                        Add
                    </button>
                </div>

                <div className="rmx object-card-collection">
                    {friends}
                </div>
            </SiteWithSidebar>
        );
    },
    addFriend: function() {
        var accountId = this.refs.accountId.getDOMNode().value;
        if (accountId) {
            this.props.account.content.friends.push(accountId);
        }
    }
});

rmx.Components.Binding = React.createClass({
    render: function() {
        var bindings = this.props.account.content.bindings
          , action   = this.props.action
          , binding  = bindings.findBinding(action) || new rmx.Storage.Binding
          , secondaryTriggerDisabled = binding.primaryTrigger == '';

        return (
            <tr>
                <td>{action}</td>
                <td><input value={binding.primaryTrigger} onChange={this.changePrimaryTrigger}/></td>
                <td><input value={binding.secondaryTrigger} disabled={secondaryTriggerDisabled} onChange={this.changeSecondaryTrigger}/></td>
            </tr>
        );
    },
    changePrimaryTrigger: function(ev) {
        var account     = this.props.account.content
          , action      = this.props.action
          , binding     = account.bindings.findBinding(action) || new rmx.Storage.Binding
          , prevTrigger = binding.primaryTrigger
          , trigger     = ev.target.value;

        this.updateTrigger(account.bindings, action, prevTrigger, trigger);
    },
    changeSecondaryTrigger: function(ev) {
        var account     = this.props.account.content
          , action      = this.props.action
          , binding     = account.bindings.findBinding(action) || new rmx.Storage.Binding
          , prevTrigger = binding.secondaryTrigger
          , trigger     = ev.target.value;

        this.updateTrigger(account.bindings, action, prevTrigger, trigger);
    },
    updateTrigger: function(bindings, action, previousTrigger, trigger) {
        // Clear the trigger if it's currently attached to a different action.
        bindings.bindings.forEach(function(binding) {
            if (trigger && binding.action !== action) {
                rmx.data.removeItem(binding.triggers, trigger);
            }
        });

        var binding = bindings.findBinding(action);
        if (binding) {
            var index = binding.triggers.indexOf(previousTrigger);
            if (index > -1) {
                binding.triggers.splice(index, 1, trigger);
            } else {
                binding.triggers.push(trigger);
            }
        } else {
            binding = Avers.mk(Binding, { action: action, triggers: [trigger] });
            rmx.data.pushItem(bindings.bindings, binding);
        }
    }
});

rmx.Components.AccountBindings = React.createClass({
    render: function() {
        var account = this.props.account
          , Binding = rmx.Components.Binding;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <table>
                    <thead>
                        <tr>
                            <th colSpan="3">
                                <h1>Movement</h1>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <Binding account={account} action="move-forward" />
                        <Binding account={account} action="move-backward" />
                    </tbody>

                    <thead>
                        <tr>
                            <th colSpan="3">
                                <h1>Action Buttons</h1>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <Binding account={account} action="action-button-1" />
                        <Binding account={account} action="action-button-2" />
                        <Binding account={account} action="action-button-3" />
                        <Binding account={account} action="action-button-4" />
                        <Binding account={account} action="action-button-5" />
                        <Binding account={account} action="action-button-6" />
                        <Binding account={account} action="action-button-7" />
                        <Binding account={account} action="action-button-8" />
                        <Binding account={account} action="action-button-9" />
                    </tbody>

                    <thead>
                        <tr>
                            <th colSpan="3">
                                <h1>Combat</h1>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <Binding account={account} action="cancel-action" />
                        <Binding account={account} action="target-nearest-enemy" />
                    </tbody>

                </table>
            </SiteWithSidebar>
        );
    }
});

var AccountFriend = React.createClass({
    render: function() {
        var id        = this.props.id
          , avatarUrl = rmx.Core.accountAvatarUrl(id)
          , nick      = rmx.data.resolveReferenceString(id + ':login').get(id);

        return (
            <div className="rmx account-badge">
                <i className="remove icon link" onClick={this.remove}></i>
                <img src={avatarUrl} />
                <div className="nick">{nick}</div>
            </div>
        );
    },
    remove: function() {
        rmx.data.removeItem(this.props.account.content.friends, this.props.id);
    }
});


var TickTimer = React.createClass({
    render: function() {
        var tickTimer = this.props.tickTimer;

        return (
            <div>
                <select value={tickTimer.type} onChange={this.changeType}>
                    <option value='interval'>interval</option>
                    <option value='counter'>counter</option>
                </select>
                <div className="form-row">
                    <div className="form-row-element">
                        <label>min</label>
                        <NumberInput object={tickTimer.content} field="min" />
                    </div>
                    <div className="form-row-element">
                        <label>base</label>
                        <NumberInput object={tickTimer.content} field="base" />
                    </div>
                    <div className="form-row-element">
                        <label>max</label>
                        <NumberInput object={tickTimer.content} field="max" />
                    </div>
                </div>
            </div>
        );
    },
    changeType: function(ev) {
        switch (ev.target.value) {
        case 'interval': return this.props.tickTimer.content = Avers.mk(rmx.Storage.Interval, {});
        case 'counter':  return this.props.tickTimer.content = Avers.mk(rmx.Storage.Counter, {});
        }
    }
});

rmx.Components.AuraMainView = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , aura      = resource.content;

        var sidebarItems = rmx.Views.auraSidebarItems(encounter, resource.id);

        var auraCategories = rmx.Storage.auraCategoryNames.map(function(name) {
            return <option value={name} key={name}>{name}</option>;
        });

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={aura} field="name" />

                    <label>description</label>
                    <Text object={aura} field="description" />

                    <label>icon</label>
                    <ObjIdPicker object={aura} field="iconId" searchResult={rmx.data.icons.references} />

                    <label>category</label>
                    <select value={aura.category} onChange={this.onCategoryChange}>
                        {auraCategories}
                    </select>

                    <FormSectionHeader header="energy" />
                    <p>
                        The amount of energy the aura starts with. Can be zero
                        to indicate that the aura doesn't use energy.
                    </p>
                    <NumberInput object={aura} field="energy" />

                    <FormSectionHeader header="stack count" />
                    <p>
                        The number of stacks the aura starts with. Can be zero
                        to indicate that the aura doesn't use stacks.
                    </p>
                    <NumberInput object={aura} field="stackCount" />

                    <div className="form-row">
                        <div className="form-row-element">
                            <label>Uniqueness Constraint</label>
                            <input disabled value={aura.uniquenessConstraint.condition}></input>
                        </div>
                        <div className="form-row-element">
                            <label>Violation Resolver</label>
                            <input disabled value={aura.uniquenessConstraint.violationResolver.source}></input>
                        </div>
                    </div>

                    <FormSectionHeader header="duration" />
                    <p>
                        If enabled, the aura will fade from the holder after the duration elapses.
                    </p>
                    <Optional opt={aura.duration}>
                        <div className="form-row">
                            <div className="form-row-element">
                                <label>min</label>
                                <NumberInput object={aura.duration.content} field="min" />
                            </div>
                            <div className="form-row-element">
                                <label>base</label>
                                <NumberInput object={aura.duration.content} field="base" />
                            </div>
                            <div className="form-row-element">
                                <label>max</label>
                                <NumberInput object={aura.duration.content} field="max" />
                            </div>
                        </div>
                    </Optional>

                    <FormSectionHeader header="tick timer" />
                    <p>
                        The aura ticks either in a fixed interval, or ticks a given number of times (counter). You can specify spell effects which should be applied at each tick.
                    </p>
                    <TickTimer tickTimer={aura.tickTimer} />
                </div>
            </EncounterResourcePageWithSidebar>
        );
    },
    onCategoryChange: function(ev) {
        var resource = this.props.resource
          , aura     = resource.content;

        aura.category = ev.target.value;

        switch (aura.category) {
        case 'none':
            break;

        case 'flask':
        case 'total-damage-immunity':
            aura.uniquenessConstraint.useTemplate('global-replace');
            break;

        case 'potion':
        case 'damage-absorbing-shield':
            aura.uniquenessConstraint.useTemplate('local-replace');
            break;

        case 'orb':
            aura.uniquenessConstraint.useTemplate('local-stacking');
            break;

        default:
            console.error('Unhandled aura category:', aura.category);
        }
    }
});

rmx.Components.AuraEffectState = React.createClass({
    render: function() {
        var Reference = rmx.Components.Reference
          , effect = this.props.effect;

        return (
            <div className="form-row">
                <div className="form-row-element">
                    <label>visual</label>
                    <ObjIdPicker object={effect} field="particleId" searchResult={rmx.data.particleeffects.references} />
                </div>
                <div className="form-row-element">
                    <label>sound</label>
                    <ObjIdPicker object={effect} field="soundId" searchResult={rmx.data.sounds.references}/>
                </div>
                <div className="form-row-element">
                    <label>animation</label>
                    <select value={effect.animationId} onChange={this.changeType}>
                        <option value=''>none</option>
                        <option value='hit'>hit</option>
                    </select>
                </div>
            </div>
        );
    },
    changeType: function(ev) {
        this.props.effect.animationId = ev.target.value;
    }
});

rmx.Components.AuraSensousView = React.createClass({
    render: function() {
        var encounter   = this.props.encounter
          , resource    = this.props.resource
          , aura        = resource.content
          , metaEffects = aura.sensousEffects;

        var sidebarItems = rmx.Views.spellSidebarItems(encounter, resource.id);

        var AuraEffectState = rmx.Components.AuraEffectState;

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}
                vertical>
                <div className="form aura">
                    <label>Aura begin</label>
                    <AuraEffectState effect={metaEffects.auraStart} />
                </div>

                <div className="form aura">
                    <label>Aura steady</label>
                    <AuraEffectState effect={metaEffects.auraSteady} />
                </div>

                <div className="form aura">
                    <label>Aura tick</label>
                    <AuraEffectState effect={metaEffects.auraTick} />
                </div>

                <div className="form aura">
                    <label>Aura end</label>
                    <AuraEffectState effect={metaEffects.auraEnd} />
                </div>

            </EncounterResourcePageWithSidebar>
        );
    }
});

var SpellEffectImmunity = React.createClass({
    render: function() {
        var content = this.props.content;

        return (
            <Expression expr={content.content} env='SpellEffectImmunity' />
        );
    }
});

var AbsorbSpellDamage = React.createClass({
    render: function() {
        var content = this.props.content;

        return (
            <div className="vertical form">
                <label>predicate</label>
                <Expression expr={content.predicate.content} env='AbsorbSpellDamagePredicate' />

                <label>amount</label>
                <Expression expr={content.amount.content} env='AbsorbSpellDamageAmount' />

                <label>action</label>
                <Expression expr={content.action.content} env='AbsorbSpellDamageAction' />
            </div>
        );
    }
});

var AttributeModifier = React.createClass({
    render: function() {
        var content = this.props.content;

        return (
            <div className="form">
                <label>name</label>
                <Input object={content} field="name" />

                <Expression expr={content.content} env='AttributeModifier' />
            </div>
        );
    }
});

var Possess = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , content   = this.props.content;

        var searchResult = rmx.Core.resourceSearchResult(encounter, function(res) {
            return res.type === 'behavior';
        });

        return (
            <div className="form">
            </div>
        );
    }
});

var SubstituteBehavior = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , content   = this.props.content;

        var searchResult = rmx.Core.resourceSearchResult(encounter, function(res) {
            return res.type === 'behavior';
        });

        return (
            <div className="form">
                <label>behavior</label>
                <ReferencePicker reference={content.behavior} searchResult={searchResult} toPath={rmx.paths.toResourcePath} />
            </div>
        );
    }
});

var Taunt = React.createClass({
    render: function() {
        return (
            <div className="form">
                Taunt forces the aura holder to attack the aura caster.
            </div>
        );
    }
});

function auraEffectContent(encounter, type, content) {
    var props = { encounter: encounter, content: content };

    switch (type) {
    case 'absorb-spell-damage'   : return React.createElement(AbsorbSpellDamage, props);
    case 'attribute-modifier'    : return React.createElement(AttributeModifier, props);
    case 'possess'               : return React.createElement(Possess, props);
    case 'spell-effect-immunity' : return React.createElement(SpellEffectImmunity, props);
    case 'substitute-behavior'   : return React.createElement(SubstituteBehavior, props);
    case 'taunt'                 : return React.createElement(Taunt, props);
    default                      : return <div>Unknown type: {type}</div>;
    }
}

var AuraEffect = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , effect    = this.props.effect
          , type      = Avers.toJSON(effect).type;

        return (
            <div className="rmx aura-effect">
                <CollectionItemHeader id={effect.id} text={type} onRemove={this.removeEffect} />
                {auraEffectContent(encounter, type, effect.effect)}
            </div>
        );
    },
    removeEffect: function() {
        rmx.data.removeItem(this.props.owner, this.props.effect);
    }
});

rmx.Components.AuraEffectsView = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , aura      = resource.content;

        var sidebarItems = rmx.Views.auraSidebarItems(encounter, resource.id);

        var effects = aura.effects.map(function(x) {
            return <AuraEffect key={x.id} owner={aura.effects} encounter={encounter} effect={x} />;
        });

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}
                vertical>
                <div>
                    <CreateEffectButtons
                        collection={aura.effects}
                        effectConstructor={rmx.Storage.AuraEffect}
                        effectTypes={rmx.Core.auraEffects()} />

                    {effects}
                </div>
            </EncounterResourcePageWithSidebar>
        );
    }
});

var EventHandler = React.createClass({
    render: function() {
        var eventHandler = this.props.eventHandler
          , event        = eventHandler.event
          , content      = eventHandler.content;

        return (
            <div className="rmx event-handler">
                <CollectionItemHeader id={eventHandler.id} text={event} onRemove={this.remove} />
                <Expression expr={content} env='Global' />
            </div>
        );
    },
    remove: function() {
        rmx.data.removeItem(this.props.collection, this.props.eventHandler);
    }
});

rmx.Components.AuraEventHandlers = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , aura      = resource.content;

        var sidebarItems = rmx.Views.auraSidebarItems(encounter, resource.id);

        var eventHandlers = aura.eventHandlers.map(function(x) {
            return <EventHandler collection={aura.eventHandlers} eventHandler={x} key={x.id} />;
        });

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}
                vertical>
                <h1>Event Handlers</h1>

                <div>
                    <button className="small primary button" onClick={this.createEventHandler.bind(this, 'aura-tick')}>aura tick</button>
                </div>

                {eventHandlers}
            </EncounterResourcePageWithSidebar>
        );
    },
    createEventHandler: function(event) {
        var resource = this.props.resource
          , aura     = resource.content;

        var eventHandler = Avers.mk(rmx.Storage.EventHandler, { event: event, type: 'expression', content: {} });
        rmx.data.pushItem(aura.eventHandlers, eventHandler);
    }
});

rmx.Components.CreatureMainView = React.createClass({
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource;

        var creature = resource.content;

        var sidebarItems = rmx.Views.creatureSidebarItems(encounter, resource.id);

        return (
            <EncounterResourcePageWithSidebar
                encounter={encounter}
                resource={resource}
                sidebarItems={sidebarItems}>

                <div className="form">
                    <label>name</label>
                    <Input object={creature} field="name" />

                    <label>health</label>
                    <div className="form-row">
                        <div className="form-row-element">
                            <label>min</label>
                            <NumberInput object={creature.health} field="min" />
                        </div>
                        <div className="form-row-element">
                            <label>max</label>
                            <NumberInput object={creature.health} field="max" />
                        </div>
                    </div>

                    <label>model</label>
                    <ReferencePicker reference={creature.model}
                        searchResult={rmx.data.models.references} toPath={rmx.paths.toResourcePath} />

                    <label>skin</label>
                    <Input object={creature} field="skinId" />

                    <label>behavior</label>
                    <ReferencePicker reference={creature.behavior}
                        searchResult={rmx.Core.behaviorSearchResult(encounter)} toPath={rmx.paths.toResourcePath} />

                    <label>faction</label>
                    <Input object={creature} field="faction" />
                </div>
            </EncounterResourcePageWithSidebar>
        );
    }
});

rmx.Components.SoundMainView = React.createClass({
    render: function() {
        var sound = this.props.sound;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={sound.content} field="name" />

                    <BlobUploader object={sound.content} field='blobId' />
                    <rmx.Components.SoundPlayer sound={sound.content} />
                </div>
            </SiteWithSidebar>
        );
    }
});

rmx.Components.SoundPlayer = React.createClass({
    render: function() {
        var sound = this.props.sound;
        var soundBlobUrl = rmx.blobUrl(sound.blobId);

        return (
            <audio src={soundBlobUrl} controls />
        );
    }
});

rmx.Components.ModelAnimationsView = React.createClass({
    render: function() {
        var model = this.props.model;
        var animations = model.animationLabels.map(function(x) {
            return <rmx.Components.AnimationLabel animationLabels={model.animationLabels} animationLabel={x} key={x.id} />
        });

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <h1>
                    Animations
                    <button className="button" onClick={this.createAnimation}>
                        <i className="icon-plus"></i>
                    </button>
                </h1>

                {animations}
            </SiteWithSidebar>
        );
    },
    createAnimation: function() {
        var animationLabel = rmx.Storage.AnimationLabel.mk('default');
        rmx.data.pushItem(this.props.model.animationLabels, animationLabel);
    }
});

rmx.Components.AnimationLabel = React.createClass({
    render: function() {
        var label = this.props.animationLabel;

        return (
            <div className="form">
                <div className="form-row">
                    <div className="form-row-element">
                        <label>name</label>
                        <Input className="fluid" object={label} field="name" />
                    </div>
                    <div className="form-row-element">
                        <label>start</label>
                        <NumberInput object={label} field="start" />
                    </div>
                    <div className="form-row-element">
                        <label>end</label>
                        <NumberInput object={label} field="end" />
                    </div>
                    <div className="form-row-element">
                        <label>fps</label>
                        <NumberInput object={label} field="fps" />
                    </div>
                    <div className="form-row-element">
                        <label>button</label>
                        <button className="button danger" style={{margin:0}} onClick={this.deleteAnimationLabel}>remove</button>
                    </div>
                </div>
            </div>
        );
    },
    deleteAnimationLabel: function() {
        rmx.data.removeItem(this.props.animationLabels, this.props.animationLabel);
    }
});



rmx.Components.ModelViewer = React.createClass({
    getInitialState: function() {
        return { animationName: null }
    },
    render: function() {
        var model = this.props.model;

        var self = this;
        var animationLabels = model.animationLabels.map(function(x) {
            var className = 'animation-label';
            if (x.name == self.state.animationName) {
                className += ' selected';
            }

            function selectAnimationLabel() {
                self.setState({ animationName: x.name });
            }

            return (
                <div className={className} onClick={selectAnimationLabel} key={x.id}>
                    {x.name}
                </div>
            );
        });

        var animLabels = null;
        if (animationLabels.length > 1) {
            animLabels = <div className="animation-labels">{animationLabels}</div>;
        }

        return (
            <div className="rmx model viewer">
                <ModelCanvas model={this.props.model} animationName={this.state.animationName} />
                {animLabels}
            </div>
        );
    }
});

rmx.Components.ModelMainView = React.createClass({
    render: function() {
        var model = this.props.model;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form" style={{flexGrow:0,flexBasis:'auto'}}>
                    <label>name</label>
                    <Input object={model} field="name" />
                </div>

                <rmx.Components.ModelViewer model={model} />
            </SiteWithSidebar>
        );
    }
});

rmx.Components.File = React.createClass({
    render: function() {
        var file = this.props.file;

        var style = { display: 'inline-block', marginBottom: 0 }

        return (
            <div>
                <CollectionItemHeader id={file.id} text={file.name} onRemove={this.remove} />
                <div className="form">
                    <label>name</label>
                    <Input object={file} field="name" />

                    <label>blob</label>
                    <BlobUploader object={file} field='blobId' />
                </div>
            </div>
        );
    },
    remove: function() {
        rmx.data.removeItem(this.props.files, this.props.file);
    }
});

rmx.Components.ModelGeometryView = React.createClass({
    render: function() {
        var model = this.props.model;
        var files = model.files.map(function(x) {
            return <rmx.Components.File files={model.files} file={x} key={x.id} />
        });

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <button className="button" onClick={this.createFile}>Create File</button>
                {files}
            </SiteWithSidebar>
        );
    },
    createFile: function() {
        rmx.data.pushItem(this.props.model.files, rmx.Storage.File.mk('new file'));
    }
});

rmx.Components.SkinTexture = React.createClass({
    render: function() {
        var texture = this.props.texture;
        var blobUrl = rmx.blobUrl(texture.blobId);

        return (
            <div className="texture">
                <Input className="fluid" object={texture} field="type" />

                <div className="image">
                    <img src={blobUrl} />
                    <a className="delete ui corner label" onClick={this.deleteTexture}>
                        <i className="delete icon"></i>
                    </a>
                </div>

                <BlobUploader object={texture} field='blobId' />
            </div>
        );
    },
    deleteTexture: function() {
        rmx.data.removeItem(this.props.textures, this.props.texture);
    }
});

rmx.Components.Skin = React.createClass({
    render: function() {
        var skin = this.props.skin;
        var textures = skin.textures.map(function(x) {
            return <rmx.Components.SkinTexture textures={skin.textures} texture={x} key={x.id} />
        });

        return (
            <div className="rmx skin">
                <CollectionItemHeader id={skin.id} text={skin.name} onRemove={this.deleteSkin} />

                <div className="form">
                    <label>name</label>
                    <Input className="fluid" object={skin} field="name" />
                </div>

                <h4>
                    Textures
                    <button className="button" onClick={this.createTexture}>
                        <i className="icon-plus"></i>
                    </button>
                </h4>
                <div className="textures">
                    {textures}
                </div>
            </div>
        );
    },
    deleteSkin: function() {
        rmx.data.removeItem(this.props.skins, this.props.skin);
    },
    createTexture: function() {
        rmx.data.pushItem(this.props.skin.textures, rmx.Storage.Texture.mk(''));
    }
});

rmx.Components.ModelSkinsView = React.createClass({
    render: function() {
        var model = this.props.model;
        var skins = model.skins.map(function(x) {
            return <rmx.Components.Skin skins={model.skins} skin={x} key={x.id} />
        });

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <h1>
                    Skins
                    <button className="button" onClick={this.createSkin}>
                        <i className="icon-plus"></i>
                    </button>
                </h1>

                <div className="ui items">
                    {skins}
                </div>
            </SiteWithSidebar>
        );
    },
    createSkin: function() {
        var skin = rmx.Storage.Skin.mk('default');

        // Push one texture into the skin, we should remove this
        // and instead provide a nice UI to create textures in a skin.
        rmx.data.pushItem(skin.textures, rmx.Storage.Texture.mk('lt_diff-medium.jpg'));

        rmx.data.pushItem(this.props.model.skins, skin);
    }
});


rmx.Components.TileMainView = React.createClass({
    render: function() {
        var tile = this.props.tile;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={tile} field="name" />

                    <label>size</label>
                    <VectorInput object={tile} field="size" />
                </div>
            </SiteWithSidebar>
        );
    }
});

rmx.Components.TileSurfaceView = React.createClass({
    render: function() {
        var tile = this.props.tile;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <p>
                        The surface editor is quite primitive at the moment. Later on we'll
                        have a nice graphical interface for editing. Or possibly allow the
                        shape to be included in the model geometry and extracted automatically
                        into the tile.
                    </p>

                    <label>vertices</label>
                    <VectorInput object={tile.surface} field="vertices" />

                    <label>faces</label>
                    <VectorInput object={tile.surface} field="faces" />
                </div>
            </SiteWithSidebar>
        );
    }
});

rmx.Components.ProfileSettings = React.createClass({
    render: function() {
        var login = rmx.Core.accountLogin(rmx.data.session.accountId);

        function changeLogin(ev) {
            var acc = rmx.data.objectContent(rmx.data.session.accountId).get();
            if (acc) {
                acc.login = ev.target.value;
            }
        }

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <label>account id</label>
                    <input type="text" readOnly disabled value={rmx.data.session.accountId} />

                    <label>login</label>
                    <input type="text" value={login} onChange={changeLogin} />
                </div>
            </SiteWithSidebar>
        );
    }
});

rmx.Components.TerrainLayoutView = React.createClass({
    getInitialState: function() {
        return { activeTab: 'tiles' };
    },
    render: function() {
        var encounter = this.props.encounter
          , resource  = this.props.resource
          , terrain   = resource.content;

        var terrainEditor = this.props.terrainEditor;
        var model = terrainEditor.selectedTile ? terrainEditor.selectedTile.content.model : null;

        var tab = null;
        if (this.state.activeTab === 'tiles') {
            tab = (
                <div>
                    <div className="header">Selected Tile</div>
                    <ModelCanvas className={"tile preview"} model={model} />

                    <div className="rmx tile toolbox">
                        <rmx.Components.TileToolbox terrainEditor={terrainEditor} />
                    </div>
                </div>
            )
        } else if (this.state.activeTab === 'waypoints') {
            tab = (
                <div className="rmx tile toolbox">
                    <rmx.Components.WaypointToolbox terrainEditor={terrainEditor} />
                </div>
            );
        } else if (this.state.activeTab === 'pointsOfInterest') {
            tab = (
                <div className="rmx tile toolbox">
                    <rmx.Components.PointOfInterestToolbox terrainEditor={terrainEditor} />
                </div>
            );
        }

        return (
            <EncounterResourcePage
                encounter={encounter}
                resource={resource}>

                <div className="rmx terrain-editor-sidebar">
                    <div className="rmx buttons">
                        <div className="small rmx button" onClick={this.showTilesTab}>tiles</div>
                        <div className="small rmx button" onClick={this.showWaypointsTab}>waypoints</div>
                        <div className="small rmx button" onClick={this.showPointsOfInterest}>POIs</div>
                    </div>
                    {tab}
                </div>

                <div className="content">
                    <rmx.Components.TerrainEditor terrainEditor={terrainEditor} />
                </div>
            </EncounterResourcePage>
        );
    },
    showTilesTab: function() {
        this.setState({ activeTab: 'tiles' });
    },
    showWaypointsTab: function() {
        this.setState({ activeTab: 'waypoints' });
    },
    showPointsOfInterest: function() {
        this.setState({ activeTab: 'pointsOfInterest' });
    }
});

rmx.Components.WaypointToolbox = React.createClass({
    render: function() {
        var terrainEditor = this.props.terrainEditor;

        var waypoints = [];
        if (terrainEditor.terrain.paths.length > 0) {
            waypoints = terrainEditor.terrain.paths[0].waypoints
                        .map(function(x, idx) {

                function removeWaypoint() {
                    rmx.data.removeItem(terrainEditor.terrain.paths[0].waypoints, x);
                }

                return (
                    <div onClick={removeWaypoint}>
                        {idx}. {x.id}
                    </div>
                );
            });
        }

        var buttonClassName = "small fluid button";
        if (this.props.terrainEditor.selectedWaypoint) {
            buttonClassName += " primary";
        }

        return (
            <div>
                <button className={buttonClassName}
                        onClick={this.toggleWaypoint}>add waypoints</button>
                {waypoints}
            </div>
        );
    },
    toggleWaypoint: function() {
        this.props.terrainEditor.toggleWaypoint();
    }
});

rmx.Components.PointOfInterestToolbox = React.createClass({
    render: function() {
        var terrainEditor = this.props.terrainEditor;

        var pois = terrainEditor.terrain.pointsOfInterest .map(function(x, idx) {
            function remove() {
                rmx.data.removeItem(terrainEditor.terrain.pointsOfInterest, x);
            }

            return (
                <div>
                    {idx}. {x.id}
                    <Input object={x} field="name" />
                    <button className="small danger button" onClick={remove}>remove</button>
                </div>
            );
        });

        var buttonClassName = "small fluid button";
        if (this.props.terrainEditor.selectedPOI) {
            buttonClassName += " primary";
        }

        return (
            <div>
                <button className={buttonClassName}
                        onClick={this.toggle}>add POI</button>
                {pois}
            </div>
        );
    },
    toggle: function() {
        this.props.terrainEditor.togglePOI();
    }
});

rmx.Components.TileToolbox = React.createClass({
    render: function() {
        var terrainEditor = this.props.terrainEditor;
        var tiles = rmx.data.tiles.ids.get([]).map(function(x) {
            var isSelected = terrainEditor.selectedTile && terrainEditor.selectedTile.objectId == x;

            var className = classNames(
                { tile : 1
                , selected : isSelected
                }
            );

            var name = rmx.data.parseReferenceString(x).bind(rmx.data.displayName).get(x);

            function selectTile() {
                rmx.data.findById(x).fmap(function(entity) {
                    terrainEditor.selectTile(entity);
                }).get();
            }

            return (
                <div className={className} onClick={selectTile}>
                    {name}
                </div>
            );
        });

        var buttonClassName = "small fluid button";
        if (terrainEditor.selectedTile) {
            buttonClassName += " primary"
        }

        return (
            <div>
                <button className={buttonClassName} onClick={this.deselectTile}>deselecte tile</button>
                {tiles}
            </div>
        );
    },
    deselectTile: function() {
        this.props.terrainEditor.selectTile(null);
    }
});

rmx.Components.TerrainEditor = React.createClass({
    render: function() {
        return (
            <div className="rmx terrain-editor-canvas" ref="canvas"></div>
        );
    },
    componentDidMount: function() {
        var editor = this.props.terrainEditor
          , canvas = this.refs.canvas.getDOMNode();

        editor.scene.appendTo(canvas);
        editor.inputSource.bindEvents();

        var self = this
        function render(now) {
            if (self.isMounted()) {
                requestAnimationFrame(render);
                editor.update(now);
            }
        }

        requestAnimationFrame(render);
    },
    componentWillUnmount: function() {
        var canvas = this.refs.canvas.getDOMNode();
        this.props.terrainEditor.scene.removeFrom(canvas);
        this.props.terrainEditor.inputSource.unbindEvents();
    }
});

var SkyboxTexture = React.createClass({
    render: function() {
        var texture = this.props.skybox.getTexture(this.props.pos);
        if (!texture) {
            var texture = Avers.mk(rmx.Storage.Texture, { type: this.props.pos });
            rmx.data.pushItem(this.props.skybox.textures, texture);
        }
        var textureUrl = rmx.blobUrl(texture.blobId);

        return (
            <div className="skybox texture">
                <div className="image">
                    <img src={textureUrl} />
                </div>

                <BlobUploader object={texture} field='blobId' />
            </div>
        );
    }
});

var SkyboxViewer = React.createClass({
    getInitialState: function() {
        var scene = new rmx.WebGL.Scene();
        return { scene: scene };
    },
    render: function() {
        rmx.Core.WebGL.renderStorageSkybox(this.state.scene, 'skybox', this.props.skybox);
        return <div ref="canvas"></div>;
    },
    componentDidMount: function() {
        var canvas = this.refs.canvas.getDOMNode();
        this.state.scene.appendTo(canvas);
    },
    componentWillUnmount: function() {
        var canvas = this.refs.canvas.getDOMNode();
        this.state.scene.removeFrom(canvas);
    }
});

rmx.Components.SkyboxMainView = React.createClass({
    render: function() {
        var skybox = this.props.skybox;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="form">
                    <label>name</label>
                    <Input object={skybox} field="name" />

                    <label>textures</label>
                    <div style={{ marginLeft: 200 }}>
                        <SkyboxTexture pos={'py'} skybox={skybox} />
                    </div>
                    <div style={{ display: 'flex', flexFlow: 'row wrap' }}>
                        <SkyboxTexture pos={'pz'} skybox={skybox} />
                        <SkyboxTexture pos={'px'} skybox={skybox} />
                        <SkyboxTexture pos={'nz'} skybox={skybox} />
                        <SkyboxTexture pos={'nx'} skybox={skybox} />
                    </div>
                    <div style={{ marginLeft: 200, paddingBottom: 10 }}>
                        <SkyboxTexture pos={'ny'} skybox={skybox} />
                    </div>
                </div>

                <SkyboxViewer skybox={skybox} />
            </SiteWithSidebar>
        );
    }
});

var ParticleEffectCanvas = React.createClass({
    render: function() {
        return (
            <div ref="canvas" style={{ display: 'flex', flex: '1' }}></div>
        );
    },
    componentDidMount: function() {
        var canvas   = this.refs.canvas.getDOMNode()
          , renderer = this.props.renderer;

        renderer.scene.appendTo(canvas);
        renderer.inputSource.bindEvents();

        var self = this
        function render(now) {
            if (self.isMounted()) {
                requestAnimationFrame(render);
                renderer.update(now);
            }
        }

        requestAnimationFrame(render);
    },
    componentWillUnmount: function() {
        var canvas   = this.refs.canvas.getDOMNode()
          , renderer = this.props.renderer;

        renderer.inputSource.unbindEvents();
        renderer.scene.removeFrom(canvas);
    }
});

rmx.Components.ParticleEffectViewer = React.createClass({
    getInitialState: function() {
        return { renderer: new rmx.ParticleEffectRenderer() };
    },
    render: function() {
        this.state.renderer.particleEffect = this.props.particleeffect;
        var pe = this.props.particleeffect;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <button className="fluid rmx button" onClick={this.startEffect}>
                    Start Effect
                </button>
                <ParticleEffectCanvas particleeffect={pe} renderer={this.state.renderer} />
            </SiteWithSidebar>
        );
    },
    startEffect: function() {
        this.state.renderer.startEffect();
    }
});

rmx.Components.ParticleEffectMainView = React.createClass({
    render: function() {
        var pe     = this.props.particleeffect;

        return (
            <SiteWithSidebar sidebarItems={this.props.sidebarItems}>
                <div className="vertical form">
                    <label>name</label>
                    <Input object={pe} field="name" />
                    <label>script</label>
                    <Script object={pe} field="script" />
                </div>
            </SiteWithSidebar>
        );
    }
});

})();
