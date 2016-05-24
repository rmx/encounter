
import * as Avers from '../../vendor/avers';



export class Range {

    min : number;
    max : number;

    static mk(min: number, max: number): Range {
        return Avers.mk<Range>(Range,
            { min : min
            , max : max
            }
        );
    }
}

Avers.definePrimitive(Range, 'min');
Avers.definePrimitive(Range, 'max');
