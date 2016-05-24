import { State } from '../../Game/State';
import { randomBetween } from '../../Game';
import { checkParams } from '../../Game/Env';
import { clamp } from '../../Pure/Math';



function
gaussRandom(state: State): number {
    var u = randomBetween(state, -1, +1)
      , v = randomBetween(state, -1, +1)
      , r = u * u + v * v;

    if (r === 0 || r > 1) {
        return gaussRandom(state);
    } else {
        return u * Math.sqrt(-2 * Math.log(r) / r);
    }
}


export function mk
(state: State) {
    var env: any = {};


    env.uniformDistribution =
    function(min: number, max: number): number {
        checkParams('uniformDistribution', arguments, Number, Number);
        return state.prng.randomBetween(min, max);
    };


    env.uniformSumDistribution =
    function(min: number, max: number, count: number): number {
        checkParams('uniformSumDistribution', arguments, Number, Number, Number);
        var ret = 0;
        while (count-- > 0) {
            ret += randomBetween(state, min, max);
        }
        return ret;
    };

    env.normalDistribution =
    function(mean: number, stddev: number, xsig: number): number {
        checkParams('normalDistribution', arguments, Number, Number, Number);
        var range = xsig * stddev;
        return mean + stddev * clamp(-range, +range, gaussRandom(state));
    };


    return env;
}
