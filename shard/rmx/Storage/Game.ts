
import * as Avers from '../../vendor/avers';
import { Reference } from './Reference';



export class GamePlayer {
    id     : string;
    roleId : string;
}

Avers.definePrimitive(GamePlayer, 'id');
Avers.definePrimitive(GamePlayer, 'score');



export class GameObjective {
    id          : string;
    isCompleted : boolean;
}

Avers.definePrimitive(GameObjective, 'id');
Avers.definePrimitive(GameObjective, 'isCompleted');



export class GameParty {
    id         : string;
    score      : number;
    players    : GamePlayer[];
    objectives : GameObjective[];
}

Avers.definePrimitive (GameParty, 'id');
Avers.definePrimitive (GameParty, 'score');
Avers.defineCollection(GameParty, 'players', GamePlayer);
Avers.defineCollection(GameParty, 'objectives', GameObjective);


export enum Purpose { Verification, Grading }

export class Game {
    encounter : Reference;
    purpose   : string;
    shardId   : string;
    stage     : string;
    duration  : number;
    parties   : GameParty[];
}

Avers.defineObject    (Game, 'encounter', Reference, {});
Avers.definePrimitive (Game, 'purpose');
Avers.definePrimitive (Game, 'shardId');
Avers.definePrimitive (Game, 'stage');
Avers.definePrimitive (Game, 'duration');
Avers.defineCollection(Game, 'parties', GameParty);
