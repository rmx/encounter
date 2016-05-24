/// <reference path="./events.ts" />
/// <reference path="./Types.ts" />
/// <reference path="../Pure/Ids.ts" />
/// <reference path="../Pure/Template.ts" />
/// <reference path="../Pure/Game.ts" />
/// <reference path="../Pure/Game/Events.ts" />

module rmx.Game {

    import CombatEvent = rmx.Pure.Game.CombatEvent;


    export class AnnotatedEvent<T> {
        constructor
            ( public time  : number
            , public event : T
            ) {}
    }


    // AllEventTypes
    // -------------------------------------------------------------------------
    //
    // Superset of shared events (rmx.Pure.Game.Event) and client local events
    // (those are needed to render the graphics and audio).

    export type AllEventTypes
        = rmx.Pure.Game.Event
        | AuraTickMessage
        | SpellcastFinish
        ;



    // ConsoleMessage
    // -----------------------------------------------------------------------
    //
    // An item which is shown in the console (wardrobe or ingame). Each item
    // has a timestamp (TODO: which clock?), the whole message can be in
    // in a particular color (unless overridden in the individual fragments)
    // and contains a template which can be rendered into a string or a React
    // element.

    export class ConsoleMessage {
        constructor
          ( public time     : number
          , public color    : string // CSS hex color, optional
          , public template : rmx.Pure.Template.Template
          ) {}
    }


    export class State {

        idCounter          : number;

        events             : AnnotatedEvent<AllEventTypes>[];
        consoleMessages    : ConsoleMessage[];

        // rmx.Pure.IState
        gameId             : string;
        encounterId        : string;

        stage              : rmx.Pure.Stage;
        parties            : rmx.Pure.Party[];
        entities           : Map<rmx.Pure.EntityId, rmx.Pure.IEntity>;


        constructor(gameId: string) {
            this.idCounter       = 0;

            this.events          = [];
            this.consoleMessages = [];

            this.gameId          = gameId;
            this.encounterId     = null;

            this.stage           = rmx.Pure.Stage.Setup;
            this.parties         = [];
            this.entities        = new Map<rmx.Pure.EntityId, rmx.Pure.IEntity>();
        }
    }


    // Generate a new ID, suitable to be used for entities and systems.
    export function
    newId(state: State): string {
        return 'L' + (state.idCounter++).toString();
    }

    export function
    pushConsoleMessage
    ( state    : State
    , now      : number
    , color    : string
    , template : rmx.Pure.Template.Template
    ): void {
        var msg = new ConsoleMessage(now, color, template);
        state.consoleMessages.push(msg);
    }

    export function
    pushAnnotatedEvent(state: State, event: rmx.Pure.Game.Event): void {
        state.events.push(new AnnotatedEvent(rmx.now(), event));
    }


    export function
    eventsOfType<T>
    ( state       : State
    , constructor : new(...args) => T
    ): AnnotatedEvent<T>[] {
        return <any> state.events.filter(ae => {
            // FIXME: Why not instanceof?
            return ae.event.constructor === constructor;
        });
    }


    // combatEvents
    // -----------------------------------------------------------------------
    //
    // Filter out all console messages which are of type CombatEvent.

    export function
    combatEvents(state: State): AnnotatedEvent<CombatEvent>[] {
        return eventsOfType<CombatEvent>(state, CombatEvent);
    }



    // eventsInFrame
    // -----------------------------------------------------------------------
    //
    // All events which happened within the given frame (specified by now and
    // dt since the last frame).

    export function
    eventsInFrame(state: State, now: number, dt: number): AnnotatedEvent<any>[] {
        return state.events.filter(ae => {
            return timeWithin(ae.time, now, dt);
        });
    }


    // timeWithin
    // -----------------------------------------------------------------------
    //
    // True if the given time (first argument) is within the frame specified
    // by now and dt.

    export function
    timeWithin(t: number, now: number, dt: number): boolean {
        return t > now - dt && t <= now;
    }
}
