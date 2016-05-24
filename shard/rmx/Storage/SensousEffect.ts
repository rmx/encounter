
import * as Avers from '../../vendor/avers';



export class SensousEffectState {

    particleId  : string;
    soundId     : string;
    animationId : string;
}

Avers.definePrimitive(SensousEffectState, 'particleId',  null);
Avers.definePrimitive(SensousEffectState, 'soundId',     null);
Avers.definePrimitive(SensousEffectState, 'animationId', null);
