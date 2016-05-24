/// <reference path="./Aura.ts" />
/// <reference path="./Behavior.ts" />
/// <reference path="./Class.ts" />
/// <reference path="./Creature.ts" />
/// <reference path="./Objective.ts" />
/// <reference path="./Spell.ts" />
/// <reference path="./Terrain.ts" />

module rmx.Storage {

    export enum Kind { Inline, External, Modified };
    export class Resource<T> {
        static Kind = Kind;

        id        : string;
        type      : string;
        reference : Reference;
        content   : T;


        static mkInline<T>(type: string, content: any): Resource<T> {
            return Avers.mk<Resource<T>>(Resource, {
                type    : type,
                content : content,
            });
        }

        static mkExternal<T>(type: string, reference: Reference): Resource<T> {
            return Avers.mk<Resource<T>>(Resource, {
                type      : type,
                reference : reference,
            });
        }

        static mkModified<T>(type: string, reference: Reference, content: any): Resource<T> {
            return Avers.mk<Resource<T>>(Resource, {
                type      : type,
                reference : reference,
                content   : content,
            });
        }

        get kind(): Kind {
            if (this.reference && this.content) {
                return Kind.Modified;
            } else if (this.reference) {
                return Kind.External;
            } else {
                return Kind.Inline;
            }
        }

    }

    var resourceTypes =
        { aura      : Aura
        , behavior  : Behavior
        , class     : Class
        , creature  : Creature
        , objective : Objective
        , spell     : Spell
        , terrain   : Terrain
        };

    Avers.definePrimitive(Resource, 'type');
    Avers.defineObject   (Resource, 'reference', Reference);
    Avers.defineVariant  (Resource, 'content', 'type', resourceTypes);

}
