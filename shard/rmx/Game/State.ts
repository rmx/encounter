
import { AccountId, EntityId } from '../Pure/Ids';
import { Stage, Party, IEntity } from '../Pure/Game';
import { Event } from '../Pure/Game/Events';
import { Alea } from '../Game/Random';
import { ContentProvider } from '../Game/ContentProvider';
import { Message } from '../Game/Messages';



export class State {

    error : Error;
    // ^ Initially null, set to the exception if the game crashes. If this
    // is non-null then any subsequent messages will be silently ignored.

    contentProvider     : ContentProvider;

    idCounter           : number;

    currentTime : number;
    // ^ The current time (in seconds). The epoch of this time is when
    // the state was created. So it starts out as zero and then ticks on.

    duration : number;
    // ^ During the 'Setup' stage this is zero. When the game is in the
    // 'Running' stage, this contains the time when the game started
    // running, so we can compute the current game duration. Once the game
    // transitions to 'Finished' the duration will be fixed and will not
    // change anymore. See the 'duration' function.

    scoringFunction : ScoringFunction;
    // ^ The scoring function is compiled during startup, here we keep
    // a reference to the compiled bytecode.

    prng                : Alea;
    glue                : any;

    futures             : Future[];
    // The Futures which are queued to be executed at some point in the
    // future.

    futureCounter       : number;
    // This is used to implement a stable sort of the queued futures. If
    // they have the same 'executeAt' time then the serial number (which
    // is unique) decides who comes first.

    outgoingMessages    : { msg: Message; set: AccountId[]; }[];
    // ^ List of messages which should be sent out to clients after
    // processing the current message.

    // IState
    stage               : Stage;
    parties             : Party[];
    entities            : Map<EntityId, IEntity>;


    constructor(contentProvider: ContentProvider) {
        this.error            = null;
        this.contentProvider  = contentProvider;

        this.idCounter        = 0;
        this.currentTime      = 0;
        this.duration         = 0;
        this.stage            = Stage.Setup;
        this.prng             = new Alea(contentProvider.seed);

        this.parties          = [];
        this.entities         = new Map<EntityId, any>();

        this.futures          = [];
        this.futureCounter    = 0;

        this.outgoingMessages = [];
    }

    get gameId(): string {
        return this.contentProvider.gameId;
    }

    get encounterId(): string {
        return this.contentProvider.encounterId;
    }
}



// duration
// -----------------------------------------------------------------------
//
// Return the duration of the game. This is the value that is stored in
// the database, and is taken into account when computing the overall
// score of the game.

export function duration(state: State): number {
    switch (state.stage) {
        case Stage.Setup:
            return 0;

        case Stage.Running:
            return state.currentTime - state.duration;

        case Stage.Finished:
            return state.duration;
    }
}



// Future
// -----------------------------------------------------------------------
//
// A Future is a function which is executed at some point in the future.
// The function will be given the game state. If you need to pass
// aditional state to the function, create a closure to capture those
// valus.

export interface Action {
    (state: State): void;
}

export interface Future {
    futureId  : number;
    executeAt : number;
    tag       : string;
    action    : Action;
}



// ScoringFunction
// -----------------------------------------------------------------------

export interface ScoringFunction {
    (event: Event): void;
}
