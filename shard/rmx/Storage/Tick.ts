
import * as Avers from '../../vendor/avers';



export class Interval {
    min  : number;
    base : number;
    max  : number;
}

Avers.definePrimitive(Interval, 'min',  3);
Avers.definePrimitive(Interval, 'base', 5);
Avers.definePrimitive(Interval, 'max',  7);



export class Counter {
    min  : number;
    base : number;
    max  : number;
}

Avers.definePrimitive(Counter, 'min',  3);
Avers.definePrimitive(Counter, 'base', 5);
Avers.definePrimitive(Counter, 'max',  7);


export class TickTimer<T> {
    content : T;
}

export var tickTimerTypes =
    { 'interval' : Interval
    , 'counter'  : Counter
    };

Avers.defineVariant(TickTimer, 'content', 'type', tickTimerTypes);
