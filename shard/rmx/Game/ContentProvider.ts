/// <reference path="../../lib/node.d.ts" />

import * as Avers from '../../vendor/avers';
import * as Storage from '../Storage';
import config from '../config';

var request = require('request');



export interface Binding<T> {
    reference : Storage.Reference;
    type      : string;
    value     : T;
}

export function
referenceToString(ref: Storage.Reference): string {
    var path = ref.path ? ':' + ref.path : '';
    return ref.objectId + path;
}

function parseAs(type: string, value: any): any {
    switch (type) {
    case 'aura'      : return Avers.parseJSON(Storage.Aura,      value);
    case 'behavior'  : return Avers.parseJSON(Storage.Behavior,  value);
    case 'class'     : return Avers.parseJSON(Storage.Class,     value);
    case 'creature'  : return Avers.parseJSON(Storage.Creature,  value);
    case 'encounter' : return Avers.parseJSON(Storage.Encounter, value);
    case 'model'     : return Avers.parseJSON(Storage.Model,     value);
    case 'objective' : return Avers.parseJSON(Storage.Objective, value);
    case 'spell'     : return Avers.parseJSON(Storage.Spell,     value);
    case 'terrain'   : return Avers.parseJSON(Storage.Terrain,   value);
    case 'tile'      : return Avers.parseJSON(Storage.Tile,      value);
    default          : throw new Error("Can not parse type: " + type);
    }
}

export class ContentProvider {

    resourceMap : { [id: string]: Binding<any> };


    constructor
      ( public gameId           : string
      , public encounterId      : string
      , public glue             : string
      , public resources        : Binding<any>[]
      , public parties          : Storage.Party[]
      , public terrainInstances : Storage.TerrainInstance[]
      , public scoringFunction  : Storage.ExtensibleExpression<any>
      ) {
        this.resourceMap = Object.create(null);
        resources.forEach(res => {
            res.value = parseAs(res.type, res.value);

            var id = referenceToString(res.reference);
            this.resourceMap[id] = res;
        });
    }

    get seed(): string {
        return this.gameId;
    }

    get tileMap(): { [id: string]: Storage.Tile } {
        var ret : any = {};
        for (var id in this.resourceMap) {
            var res = this.resourceMap[id];
            if (res.type === 'tile') {
                ret[id] = res.value;
            }
        }
        return ret;
    }
}



// encounterContentUrl
// -----------------------------------------------------------------------
//
// The URL from which the encounter content for the given game can be
// fetched. The URL can be overridden if an encounter Id was given through
// the process environment.

function
encounterContentUrl(gameId: string): string {
    if (config.encounterId) {
        return config.apiHost + "/encounters/" + config.encounterId + "/content";
    } else {
        return config.apiHost + "/games/" + gameId + "/encounterContent";
    }
}



// createContentProvider
// -----------------------------------------------------------------------

export function
createContentProvider
( gameId : string
, fn     : (err, contentProvider?: ContentProvider) => void
): void {
    request(encounterContentUrl(gameId), function(err, response, body) {
        if (!err && response.statusCode === 200) {
            try {
                var json = JSON.parse(body);

                var terrainInstances = json.terrainInstances.map(x => {
                    return Avers.parseJSON(Storage.TerrainInstance, x);
                });

                fn(null, new ContentProvider
                    ( gameId
                    , json.encounterId
                    , json.glue
                    , json.resources
                    , json.parties
                    , terrainInstances
                    , Avers.parseJSON<Storage.ExtensibleExpression<any>>(Storage.ExtensibleExpression, json.scoringFunction)
                    )
                );

            } catch (err) {
                fn(err);
            }
        } else if (err) {
            fn(err);
        } else {
            fn(new Error(body));
        }
    });
}
