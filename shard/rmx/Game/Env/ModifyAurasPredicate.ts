import { Aura } from '../../Pure/Game';
import { State } from '../../Game/State';
import { defineAccessor } from '../../Game/Env';



export function mk(state: State, aura: Aura): any {
    var env: any = {};

    env.module = {};

    defineAccessor(env, 'auraId', () => {
        return aura.auraId;
    });

    defineAccessor(env, 'stackCount', () => {
        return aura.stackCount;
    });

    defineAccessor(env, 'duration', () => {
        return aura.duration;
    });

    defineAccessor(env, 'timeLeft', () => {
        return aura.createdAt + aura.duration - state.currentTime;
    });


    return env;
}
