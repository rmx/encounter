/// <reference path="./Reference.ts" />

module rmx.Storage {

    export class KillParties {}
    Avers.declareConstant(KillParties);



    export class KillCreature {
        creature : Reference;
        count    : number;
    }

    Avers.defineObject   (KillCreature, 'creature', Reference, {});
    Avers.definePrimitive(KillCreature, 'count', 1);



    export class Task<T> {
        id      : string;
        content : T;
    }

    export var taskTypes =
        { 'kill-creature' : KillCreature
        , 'kill-parties'  : KillParties
        };

    Avers.defineVariant(Task, 'content', 'type', taskTypes);



    export class Objective {
        id    : string;

        tasks : Task<any>[];
        // ^ Tasks which need to be completed for the objective to be
        // completed.

        static mk() {
            return Avers.mk(Objective, {});
        }
    }

    Avers.defineCollection(Objective, 'tasks', Task);
}
