/// <reference path="../../ext/computation.ts" />

module rmx.data {

    export function mkObjectCollection(type: string, collectionName: string): Collection {
        return new Collection(collectionName, type);
    }


    export class Collection {

        private fetchedAt : number;
        private url       : string;
        private objectIds : string[];
        private type      : string;

        constructor(collectionName: string, type?: string) {
            this.fetchedAt = 0;
            this.url       = rmx.apiUrl('/collection/' + collectionName);
            this.objectIds = [];
            this.type      = type;
        }

        private mergeIds(ids: string[]): void {
            var isChanged = ids.reduce((a, id, index) => {
                return a || id != this.objectIds[index];
            }, false);

            if (isChanged) {
                this.objectIds = ids;
                rmx.data.startNextGeneration();
            }
        }

        private fetch(): void {
            var now = Date.now();
            if (now - this.fetchedAt > 10 * 1000) {
                this.fetchedAt = now;

                request.get(this.url).withCredentials().end((res: any) => {
                    this.mergeIds(res.body);
                });
            }
        }

        private add(id: string): void {
            this.objectIds.unshift(id);
            rmx.data.startNextGeneration();
        }

        get ids(): Computation<string[]> {
            this.fetch();
            return new Computation(() => {
                return this.objectIds;
            });
        }

        get references(): Computation<rmx.Storage.Reference[]> {
            return this.ids.fmap(ids => {
                return ids.map(rmx.data.objIdReference);
            });
        }

        create(content: any): Promise<string> {
            // TODO: Propagate errors to the promise.

            return new Promise((resolve, reject) => {
                var url  = rmx.apiUrl('/objects')
                  , json = { type: this.type, content: content };

                request.post(url).send(json).withCredentials().end((res: any) => {
                    this.add(res.body.id);
                    resolve(res.body.id);
                });
            });
        }
    }



    export class KeyedCollection<T> {

        private cache : Map<string, Collection>;

        constructor(private keyFn: (key: T) => string) {
            this.cache = new Map<string, Collection>();
        }

        get(keyInput: T): Collection {
            var key        = this.keyFn(keyInput)
              , collection = this.cache.get(key);

            if (!collection) {
                collection = new Collection(key);
                this.cache.set(key, collection);
            }

            return collection;
        }
    }
}
