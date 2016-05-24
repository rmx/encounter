import * as Storage from '../Storage';



export class Task<T> {

    constructor
      ( public id : string

        // ^ The id of the task within an objective. It is not unique
        // within a game. The id is the same as the item id in the
        // objective tasks collection.

      , public state : T
        // ^ The runtime state of the task. Is used to store state which
        // can not be computed from the game state.
      ) {}
}


// toTask
// -----------------------------------------------------------------------
//
// Create the runtime object for the given Storage task.

export function
toTask(task: Storage.Task<any>): Task<any> {
    var id = (<any>task).id;

    if (task.content instanceof Storage.KillCreature) {
        return new Task(id, new KillCreature(task.content));

    } else if (task.content instanceof Storage.KillParties) {
        return new Task(id, new KillParties());

    } else {
        throw new Error("toTask: Unknown content");
    }
}



// -----------------------------------------------------------------------
export class KillCreature {

    public creatureId    : string;
    public requiredCount : number;
    public count         : number;

    constructor(def: Storage.KillCreature) {
        // FIXME: Use referenceToString() instead of toString().
        this.creatureId    = def.creature.toString();
        this.requiredCount = def.count;
        this.count         = 0;
    }
}

export class KillParties {
}
