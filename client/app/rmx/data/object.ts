declare var request : any;

module rmx.data {

    // Response from GET /objects/:objectId
    interface IObjectResponseBody {
        id             : string; // Rename to objectId?
        type           : string;
        createdAt      : string;
        createdBy      : string;
        revisionId     : number;
        content        : any;
    }

    export enum Status { Empty, Loading, Loaded, Failed }

    export class Object<T> {

        status : Status = Status.Empty;

        promise : Promise<Object<T>>;
        // ^ Promise which resolves once the object is loaded. The value of
        // this promise is the object itself. May be rejected if the loading
        // fails (eg. due to a server error, or because the objectId is
        // invalid)

        type             : string;

        createdAt        : Date;

        createdBy        : rmx.Pure.AccountId;
        // ^ The primary author who created this object.

        revisionId       : number;
        // ^ The RevId as we think is the latest on the server. Local changes
        // are submitted against this RevId.

        shadowContent    : T;
        // ^ The content of the object at 'revisionId'.


        content          : T;

        submittedChanges : Avers.Operation[] = [];
        localChanges     : Avers.Operation[] = [];


        constructor(public objectId: string) {
            // Callbacks which are bound to this particular instance.
            this.save     = <any> _.debounce(this.save.bind(this), 500);
            this.onChange = this.onChange.bind(this);
        }


        applyToShadow(path: string, operation: any): void {
            Avers.applyOperation(this.shadowContent, path, operation);
            Avers.deliverChangeRecords(this.shadowContent);

            // Compare content and shadowContent in JSON representation.
            var contentJSON       = Avers.toJSON(this.content);
            var shadowContentJSON = Avers.toJSON(this.shadowContent);
            if (!_.isEqual(contentJSON, shadowContentJSON)) {
                // This doesn't work if two or more changes are applied in the same
                // batch.

                //throw new Error('error');
            }
        }

        onChange(changes: Avers.Change<any>[]): void {
            changes.forEach(change => {
                var op = Avers.changeOperation(change);
                this.localChanges.push(op);
                numUnsavedChanges++;
            });

            this.save();
        }

        delete(): void {
            var url = rmx.apiUrl('/objects/' + this.objectId);
            request.del(url).withCredentials().end((res: any) => {
                console.log('Deleted', this.objectId, res.status);
            });
        }

        save(): Object<T> {
            if (this.status === Status.Loaded && this.localChanges.length > 0 && this.submittedChanges.length === 0) {
                var data = { objectId       : this.objectId
                           , revisionId     : this.revisionId
                           , operations     : filterOperations(this.localChanges)
                           };

                this.submittedChanges = this.localChanges;
                this.localChanges     = [];

                rmx.data.numSavingEntities++;

                var url = rmx.apiUrl('/objects/' + this.objectId);
                request.patch(url).withCredentials().send(data).end((res: any) => {
                    if (res.status === 200) {
                        var body = res.body;

                        console.log(
                            [ 'Saved '
                            , body.resultingPatches.length
                            , ' operations on '
                            , data.objectId
                            , ' ('
                            , body.previousPatches.length
                            , ' previous patches)'
                            ].join('')
                        );

                        numUnsavedChanges -= this.submittedChanges.length;
                        numSavingEntities--;

                        body.previousPatches.forEach(patch => {
                            var op = patch.operation;
                            Avers.applyOperation(this.shadowContent, op.path, op);
                        });
                        body.resultingPatches.forEach(patch => {
                            var op = patch.operation;
                            Avers.applyOperation(this.shadowContent, op.path, op);
                        });
                        this.localChanges.forEach(op => {
                            Avers.applyOperation(this.shadowContent, op.path, op);
                        });

                        this.content = Avers.clone(this.shadowContent);
                        Avers.deliverChangeRecords(this.content);

                        Avers.attachChangeListener(this.content, this.onChange);

                        this.revisionId += body.previousPatches.length + body.resultingPatches.length;
                        this.submittedChanges = [];

                        this.save();

                    } else {
                        // The server would presumably respond with changes which
                        // were submitted before us, and we'd have to rebase our
                        // changes on top of that.

                        numSavingEntities--;

                        this.localChanges     = this.submittedChanges.concat(this.localChanges);
                        this.submittedChanges = [];
                    }

                    startNextGeneration();
                }).on('error', (err: any) => {
                    // Push the changes we've been trying to submit back into
                    // the local changes list.

                    numSavingEntities--;
                    lastErrorMessage = err.message;

                    this.localChanges     = this.submittedChanges.concat(this.localChanges);
                    this.submittedChanges = [];

                    startNextGeneration();
                });
            }

            startNextGeneration();
            return this;

            // Filter out subsequent operations which touch the same path.
            function filterOperations(ops: Avers.Operation[]): Avers.Operation[] {
                return ops.reduce(function(a: Avers.Operation[], op: Avers.Operation): Avers.Operation[] {
                    var lastOp = a[a.length - 1];

                    if (lastOp && lastOp.path == op.path && lastOp.type == 'set') {
                        a[a.length - 1] = op;
                    } else {
                        a.push(op);
                    }

                    return a;
                }, []);
            }
        }
    }



    // loadObject
    // -----------------------------------------------------------------------
    //
    // Fetch the object from the server and initialize its properties and
    // state with the response.
    //
    // Precondition: The object must have a valid 'objectId' (ie. must be a
    // non-empty string).

    export function
    loadObject<T>(obj: Object<T>): Promise<Object<T>> {
        return new Promise((resolve, reject) => {
            obj.status = Status.Loading;

            var url = rmx.apiUrl('/objects/' + obj.objectId);
            request.get(url).withCredentials().end((res: any) => {
                if (res.status === 200) {
                    resolveObjectWith<T>(obj, res.body);
                    resolve(obj);
                } else {
                    obj.status = Status.Failed;
                    reject(new Error('HTTP Status: ' + res.status));
                }
            });
        });
    }


    function
    resolveObjectWith<T>
    ( obj  : Object<T>
    , body : IObjectResponseBody
    ): void {
        obj.status         = Status.Loaded;

        obj.type           = body.type;
        obj.objectId       = body.id;
        obj.createdAt      = new Date(Date.parse(body.createdAt));
        obj.createdBy      = body.createdBy;
        obj.revisionId     = body.revisionId || 0;

        var ctor           = lookupObjectConstructor(obj.type);
        obj.content        = Avers.parseJSON<T>(ctor, body.content);
        obj.shadowContent  = Avers.parseJSON<T>(ctor, body.content);

        Avers.deliverChangeRecords(obj.content);
        Avers.deliverChangeRecords(obj.shadowContent);

        Avers.attachChangeListener(obj.content, obj.onChange);
        obj.save();

        Avers.migrateObject(obj.content);
    }



    // Patch
    // -----------------------------------------------------------------------
    //
    // A Patch is a read-only, static object. Once it is fetched from the
    // server, it will never change.

    export class Patch {
        constructor
          ( public objectId   : string
          , public revisionId : number
          , public authorId   : string
          , public createdAt  : string
          , public operation  : Avers.Operation
          ) {}
    }



    export function
    fetchPatch(objectId: string, revId: number): Promise<Patch> {
        return new Promise((resolve, reject) => {
            var url = rmx.apiUrl(
                [ '/objects'
                , objectId
                , 'patches'
                , revId
                ].join('/')
            );

            request.get(url).withCredentials().end(res => {
                if (res.status === 200) {
                    var json = res.body;

                    resolve(new Patch
                        ( objectId
                        , revId
                        , json.authorId
                        , json.createdAt
                        , json.operation
                        )
                    );

                } else {
                    reject(new Error('fetchPatch: ' + res.status));
                }
            });
        });
    }
}
