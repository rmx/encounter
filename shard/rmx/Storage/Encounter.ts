
import * as Avers from '../../vendor/avers';

import { Reference } from './Reference';
import { ExtensibleExpression } from './ExtensibleExpression';
import { Resource } from './Resource';



export class EncounterImages {
    featureSquare : string;
    marquee       : string;
    background    : string;
}

Avers.definePrimitive(EncounterImages, 'featureSquare');
Avers.definePrimitive(EncounterImages, 'marquee');
Avers.definePrimitive(EncounterImages, 'background');



export class SpawnPoint {
    terrainEntityId     : string;
    pointOfInterestName : string;
}

Avers.definePrimitive(SpawnPoint, 'terrainEntityId', '');
Avers.definePrimitive(SpawnPoint, 'pointOfInterestName', '');



export class Party {
    id         : string;
    classes    : Reference[];
    objectives : Reference[];
    spawnPoint : SpawnPoint;
}

Avers.defineCollection(Party, 'classes',    Reference);
Avers.defineCollection(Party, 'objectives', Reference);
Avers.defineObject    (Party, 'spawnPoint', SpawnPoint, {});



export class TerrainInstance {
    id      : string;

    terrain : Reference;
    // ^ Reference to the resource from which the terrain is created.
}

Avers.defineObject(TerrainInstance, 'terrain', Reference, {});



export class Encounter {

    name      : string;
    tagline   : string;
    images    : EncounterImages;
    glue      : string;

    scoringFunction : ExtensibleExpression<any>;
    // ^ Determines how the score is computed. If this is a plain
    // expression, then it will be fed events from the game.

    resources : Resource<any>[];

    classes   : Reference[];
    // ^ Deprecated. The available classes are now defined on each party.

    parties   : Avers.Collection<Party>;
    // ^ Parties which the players can choose from and join.

    terrainInstances : TerrainInstance[];
    // ^ Terrain instances which are created during initialization of the
    // encounter.


    static mk(name: string): Encounter {
        return Avers.mk<Encounter>(Encounter,
            { name : name
            }
        );
    }
}

Avers.definePrimitive (Encounter, 'name');
Avers.definePrimitive (Encounter, 'tagline');
Avers.definePrimitive (Encounter, 'description');
Avers.definePrimitive (Encounter, 'glue', 'class Glue\nmodule.exports = Glue');
Avers.defineObject    (Encounter, 'scoringFunction', ExtensibleExpression, { type: 'expression', content: {} });
Avers.defineCollection(Encounter, 'resources', Resource);
Avers.defineCollection(Encounter, 'classes', Reference);
Avers.defineCollection(Encounter, 'parties', Party);
Avers.defineCollection(Encounter, 'terrainInstances', TerrainInstance);
Avers.defineObject    (Encounter, 'images', EncounterImages, {});
