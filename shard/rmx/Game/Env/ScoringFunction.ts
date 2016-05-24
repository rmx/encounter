import { Party } from '../../Pure/Game';
import { State } from '../../Game/State';
import { setScore } from '../../Game';
import { checkParams } from '../../Game/Env';
import * as Env from '../../Game/Env';
import { clamp } from '../../Pure/Math';



import * as Global from '../Env/Global';
import * as Constructors from '../Env/Constructors';
import * as Event from '../Env/Event';
import * as Queries from '../Env/Queries';




export function mk
(state: State) {
    var env: any = {};

    env.module = {};


    env.setScore = function(party: Party, score: number): void {
        checkParams('setScore', arguments, Party, Number);
        setScore(state, party, score);
    };

    env.modifyScore = function(party: Party, diff: number): void {
        checkParams('modifyScore', arguments, Party, Number);
        var score = clamp(0, +Infinity, party.score + diff);
        setScore(state, party, score);
    };


    Env.mix(env, Global.mk(state));
    Env.mix(env, Queries.mk(state));
    Env.mix(env, Constructors.mk());
    Env.mix(env, Event.mk());


    return env;
}
