/// <reference path="../ext/computation.ts" />

/// <reference path="./Help.ts" />
/// <reference path="./Storage/Types.ts" />
/// <reference path="./LocalState.ts" />
/// <reference path="./Device.ts" />
/// <reference path="./Session.ts" />

/// <reference path="./data/object.ts" />
/// <reference path="./data/collection.ts" />

/// <reference path="./data/Static.ts" />
/// <reference path="./data/Volatile.ts" />


module rmx.data {

    // The generation number is a local counter which increments every time
    // any of the data change. It is used to re-render the UI on each change.

    export var generationNumber : number = 1;


    // Function which must be called when data which this module manages
    // changes. It is crucial that this is done properly. The UI depends on
    // these notifications in order to re-render itself correctly.

    export function
    startNextGeneration(): void {
        generationNumber++;
    }



    // device
    // -----------------------------------------------------------------------
    //
    // Data which needs to be persisted across page reloads or browser
    // restarts. Shared across all accounts, so no account-specific data
    // should be stored here. Backed by LocalStorage.

    export var device = mkDevice();
    Avers.attachChangeListener(device, startNextGeneration);

    export var preferences: Preferences;
    (<any>window).Object.defineProperty(data, 'preferences', { get: () => {
        return rmx.accountPreferences(device, <string> session.accountId);
    }});



    // session
    // -----------------------------------------------------------------------
    //
    // Describes the state of the session. This part is manipulated mainly by
    // the signup, login, logout components.

    export var session = Avers.mk<Session>(Session, {});
    Avers.attachChangeListener(session, startNextGeneration);



    // Some statistics about dirty objects, saving in progress and errors.

    export var numUnsavedChanges     : number = 0;
    export var numSavingEntities     : number = 0;
    export var numEntitiesWithErrors : number = 0;
    export var lastErrorMessage      : string = null;



    // Object type registry
    // -----------------------------------------------------------------------
    //
    // We keep track of all the different object types that can exist. The
    // constructor is used when parsing the response from the server.

    export interface ObjectConstructor {
        new(): any;
    }

    var infoTable : Map<string, ObjectConstructor> = new Map<string, ObjectConstructor>();

    export function
    registerObjectType<T>(type: string, ctor: ObjectConstructor): void {
        infoTable.set(type, ctor);
    }

    export function
    lookupObjectConstructor(type: string): ObjectConstructor {
        return infoTable.get(type);
    }




    // Entities are cached here.
    //
    // TODO: Remove entries from the cache if they are not used for a while.

    var objectCache : Map<string, Object<any>> = new Map<string, Object<any>>();



    // loadById
    // -----------------------------------------------------------------------
    //
    // Get an object by its id. If the object doens't already exist, it will
    // be registered in the local cache and fetched in the background.

    export function
    loadById<T>(id: string): Object<T> {
        var obj = objectCache.get(id);
        if (!obj) {
            obj = new Object<T>(id);
            obj.promise = new Promise((resolve, reject) => {
                loadObject(obj).then(() => {
                    Avers.attachChangeListener(obj.content, startNextGeneration);
                    resolve(obj);

                }).catch(err => {
                    console.error('loadById: Loading object from server failed', err);
                    reject(err);

                }).then(startNextGeneration);
            });

            objectCache.set(id, obj);
        }

        return obj;
    }


    // findById
    // -----------------------------------------------------------------------
    //
    // Get an object by its id. This computation is pending until the object
    // has been fully loaded.

    export function
    findById<T>(id: string): Computation<Object<T>> {
        return new Computation(() => {
            if (id) {
                var obj = loadById<T>(id);
                if (!obj.content) {
                    return <Object<T>> Computation.Pending;
                } else {
                    return obj;
                }

            } else {
                throw new Error("findById: invalid id <" + id + ">");
            }
        });
    }


    // objectContent
    // -----------------------------------------------------------------------
    //
    // Like 'findById' but unwraps the object and returns just the content.

    export function
    objectContent<T>(id: string): Computation<T> {
        return findById<T>(id).fmap(obj => {
            return obj.content;
        });
    }


    // objIdReference
    // -----------------------------------------------------------------------
    //
    // Return a reference pointing to the given ObjId. The reference always
    // points to the latest revision. By definition of an ObjId, the path
    // is empty.

    export function
    objIdReference(objId: string): rmx.Storage.Reference {
        return rmx.Storage.Reference.mk(objId, null, '');
    }


    export function
    resolveReference<T>(ref: rmx.Storage.Reference): Computation<T> {
        return findById(ref.objectId).fmap(obj => {
            var x = Avers.resolvePath(obj.content, ref.path);
            if (x === undefined) {
                throw new Error(
                    [ 'Could not resolve'
                    , ref.path
                    , 'inside object'
                    , ref.objectId
                    ].join(' ')
                );
            } else {
                return <T> x;
            }
        });
    }


    export function
    parseReferenceString(str: string): Computation<rmx.Storage.Reference> {
        return new Computation(() => {
            return rmx.Storage.Reference.parseString(str);
        });
    }

    export function
    resolveReferenceString<T>(str: string): Computation<T> {
        return parseReferenceString(str).bind<T>(resolveReference);
    }


    var patchCache : Map<string, Static<Patch>> = new Map<string, Static<Patch>>();


    // findPatch
    // -----------------------------------------------------------------------
    //
    // Note: This function does not validate the arguments. It assumes that
    // both the objectId and revisionId are valid.

    export function
    findPatch(objectId: string, revId: number): Computation<Patch> {
        return new Computation(() => {
            var key   = objectId + '@' + revId
              , patch = patchCache.get(key);

            if (!patch) {
                patch = new Static<Patch>(() => {
                    return fetchPatch(objectId, revId);
                });
                patchCache.set(key, patch);
            }

            return patch;

        }).bind(staticValue);
    }


    // Collections
    // -----------------------------------------------------------------------
    //
    // Collections are lists of objectIds. To get the actual objects, you
    // have to fetch them individually. Each collection has a corresponding
    // endpoint in the API server.
    //
    // TODO: If these lists get too big we'll have to paginate them.


    export var accounts   = mkObjectCollection('account',   'accounts');
    export var encounters = mkObjectCollection('encounter', 'encounters');
    export var icons      = mkObjectCollection('icon',      'icons');
    export var models     = mkObjectCollection('model',     'models');
    export var skyboxes   = mkObjectCollection('skybox',    'skyboxes');
    export var sounds     = mkObjectCollection('sound',     'sounds');
    export var tiles      = mkObjectCollection('tile',      'tiles');
    export var particleeffects = mkObjectCollection('particleeffect', 'particleeffects');

    export var featuredEncounters = new Collection('recommendations');
    export var recommendations    = new Collection('recommendations');
    export var newReleases        = new Collection('newReleases');
    export var releasedEncounters = new Collection('releasedEncounters');
    export var openGames          = new Collection('openGames');

    export var activeShards       = new Collection('activeShards');

    export var gamesUsingEncounter =
    new KeyedCollection((encounterId: string) => {
        return 'games/usingEncounter/' + encounterId;
    });

    export var imagesRelatedTo =
    new KeyedCollection((objId: string) => {
        return 'images/relatedTo/' + objId;
    });

    export var encounterLeaderboard =
    new KeyedCollection((encounterId: string) => {
        return 'encounter/' + encounterId + '/leaderboard';
    });


    // Top-level object types
    // -----------------------------------------------------------------------

    registerObjectType('account',        rmx.Storage.Account);
    registerObjectType('encounter',      rmx.Storage.Encounter);
    registerObjectType('game',           rmx.Storage.Game);
    registerObjectType('icon',           rmx.Storage.Icon);
    registerObjectType('image',          rmx.Storage.Image);
    registerObjectType('model',          rmx.Storage.Model);
    registerObjectType('particleeffect', rmx.Storage.ParticleEffect);
    registerObjectType('shard',          rmx.Storage.Shard);
    registerObjectType('skybox',         rmx.Storage.Skybox);
    registerObjectType('sound',          rmx.Storage.Sound);
    registerObjectType('tile',           rmx.Storage.Tile);



    // createBlob
    // -----------------------------------------------------------------------
    //
    // Upload a 'Blob' to the server. The promise will be resolved with the
    // blobId, or rejected with an error if the upload fails.
    //
    // TODO: Get the content type from the blob instead of passing it in as
    // an additional function argument.
    // See https://developer.mozilla.org/en-US/docs/Web/API/Blob.type

    export function
    createBlob(blob: Blob, contentType: string): Promise<string> {
        var xhr = new XMLHttpRequest();
        xhr.withCredentials = true;

        var promise = new Promise(function(resolve, reject) {
            xhr.open('POST', rmx.apiUrl('/blobs'));
            xhr.setRequestHeader('Content-Type', contentType);
            xhr.send(blob);

            xhr.addEventListener('readystatechange', onReadyStateChange, false);
            function onReadyStateChange() {
                if (xhr.readyState === 4) {
                    if (xhr.status === 200) {
                        var response = JSON.parse(xhr.responseText);
                        resolve(response.id);
                    } else {
                        reject(new Error(xhr.responseText));
                    }
                }
            }
        });

        // Expose the underlying XHR on the promise object. This can be used
        // by the caller to track the upload progress.
        (<any>promise).xhr = xhr;

        return promise;
    }



    // createImage
    // -----------------------------------------------------------------------
    //
    // Create an 'rmx.Storage.Image' from a 'Blob'. The promise will be
    // resolved with the image Id, or rejected with an error if the upload
    // fails.

    export function
    createImage(blob: Blob, relatedId: string): Promise<string> {
        var xhr = new XMLHttpRequest();
        xhr.withCredentials = true;

        var promise = new Promise(function(resolve, reject) {
            var suffix = relatedId ? '/' + relatedId : '';

            xhr.open('POST', rmx.apiUrl('/images' + suffix));
            xhr.setRequestHeader('Content-Type', blob.type);
            xhr.send(blob);

            xhr.addEventListener('readystatechange', onReadyStateChange, false);
            function onReadyStateChange() {
                if (xhr.readyState === 4) {
                    if (xhr.status === 200) {
                        var response = JSON.parse(xhr.responseText);
                        resolve(response.objId);
                    } else {
                        reject(new Error(xhr.responseText));
                    }
                }
            }
        });

        // Expose the underlying XHR on the promise object. This can be used
        // by the caller to track the upload progress.
        (<any>promise).xhr = xhr;

        return promise;
    }


    // changeSecret
    // -----------------------------------------------------------------------
    //
    // Change the secret (password) of the currently logged in account.

    export function
    changeSecret(secret: string): Promise<any> {
        return new Promise(function(resolve, reject) {
            request
                .post(rmx.apiUrl('/secret'))
                .withCredentials()
                .send({ secret: secret })
                .end(function(res: any) {
                    resolve(res);
                });
        });
    }



    // localState
    // -----------------------------------------------------------------------
    //
    // The local state which is used by the components to store stuff which
    // needs to be persistet across cross-page navigation.
    //
    // Eventually we can store this in 'localStorage'. But that needs careful
    // thought because localState may include state which should not be
    // preserved across full page reloads. Then it may make sense to split
    // it up into 'localState' and 'sessionState' (analogous to 'localStorage'
    // and 'sessionStorage').

    export var localState = Avers.mk<LocalState>(LocalState, {});
    Avers.attachChangeListener(localState, startNextGeneration);



    // displayName
    // -----------------------------------------------------------------------
    //
    // Attempt to extract the display name. If it can't extract a meaningful
    // display name, returns the stringified ref.

    export function
    displayName(ref: rmx.Storage.Reference): Computation<string> {
        return resolveReference<any>(ref).fmap(obj => {
            if (obj.content && obj.content.name) {
                return obj.content.name;

            } else if (obj.name) {
                return obj.name;

            } else if (obj.id) {
                return obj.id;

            } else {
                throw new Error("Could not determine display name of " + ref.toString());
            }
        });
    }


    // objectIconUrl
    // -----------------------------------------------------------------------
    //
    // Given an object, return an url to its icon. Throws an error if the object
    // does not have any icon.

    export function
    objectIconUrl(obj: any): string {
        if (obj.content && obj.content.iconId) {
            return iconUrl(obj.content.iconId);
        } else if (obj.content && obj.content.icon) {
            return iconUrl(obj.content.icon.objectId);
        } else {
            throw new Error("Object doesn't have any icon");
        }
    }


    export function
    objectIconId(obj: any): string {
        if (obj.content && obj.content.iconId) {
            return obj.content.iconId;
        } else if (obj.content && obj.content.icon) {
            return obj.content.icon.objectId;
        } else {
            throw new Error("Object doesn't have any icon");
        }
    }


    export function
    iconUrl(iconId: string): string {
        var fallbackIconUrl = '<* images/notfound.svg *>';

        if (iconId) {
            return objectContent<rmx.Storage.Icon>(iconId).fmap(icon => {
                return rmx.blobUrl(icon.blobId);
            }).get(fallbackIconUrl);

        } else {
            return fallbackIconUrl;
        }
    }


    var chars =
        [ 'abcdefghijklmnopqrstuvwxyz'
        , 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        , '1234567890'
        ].join('');

    function
    newId() {
        var id = '', length = 7;
        for (var i = 0; i < length; ++i) {
            id = id + chars[Math.floor(Math.random() * chars.length)];
        }
        return id;
    }

    export interface IHasId { id: string; }

    export function
    pushItem<T extends IHasId>(collection: T[], item: T): T {
        if (!item.id) {
            item.id = newId();
        }

        collection.push(item);
        return item;
    }

    export function
    removeItem<T>(collection: T[], item: T): void {
        var index = collection.indexOf(item);
        if (index >= 0) {
            collection.splice(index, 1);
        } else {
            console.warn
              ( 'removeItem: Item does not exist in the collection'
              , collection
              , item
              );
        }
    }



    // ShardHealth
    // -----------------------------------------------------------------------

    export interface ShardHealth {
        load : number;
    }


    var shardHealthCache = new Map<string, Volatile<ShardHealth>>();


    export function
    shardHealth(shardId: string): Computation<ShardHealth> {
        return new Computation<Volatile<ShardHealth>>(() => {
            var sm = shardHealthCache.get(shardId);

            if (!sm) {
                sm = new Volatile(() => {
                    return fetchShardHealth(shardId);
                });
                shardHealthCache.set(shardId, sm);
            }

            return sm;
        }).bind(volatileValue);
    }


    function
    fetchShardHealth(shardId: string): Promise<ShardHealth> {
        return new Promise((resolve, reject) => {
            var url = rmx.apiUrl('/shards/' + shardId + '/health');

            request.get(url).withCredentials().end(res => {
                if (res.status === 200) {
                    resolve(res.body);
                } else {
                    reject(new Error('fetchShardHealth: ' + res.status));
                }
            });
        });
    }



    // Help
    // -----------------------------------------------------------------------

    export class HelpArticle {
        constructor
          ( public articleId  : string
          , public attributes : any
          , public body       : string
          ) {}

        get title(): string {
            return this.attributes.title || this.articleId;
        }
    }



    var helpArticles = new Map<string, Static<HelpArticle>>();

    export function
    helpArticle(articleId: string): Computation<HelpArticle> {
        return new Computation(() => {
            var article = helpArticles.get(articleId);

            if (!article) {
                article = new Static<HelpArticle>(() => {
                    return fetchHelpArticle(articleId);
                });
                helpArticles.set(articleId, article);
            }

            return article;

        }).bind(staticValue);
    }


    export function
    fetchHelpArticle(articleId: string): Promise<HelpArticle> {
        return new Promise((resolve, reject) => {
            var url = rmx.Help.articleUrl(articleId);
            if (url) {
                request.get(url).withCredentials().end(res => {
                    if (res.status === 200) {
                        var json = JSON.parse(res.text);

                        resolve(new HelpArticle
                            ( articleId
                            , json.attributes
                            , json.body
                            )
                        );

                    } else {
                        reject(new Error('fetchHelpArticle: ' + res.status));
                    }
                });

            } else {
                reject(new Error('fetchHelpArticle: Invalid articleId: ' + articleId));
            }
        });
    }
}
