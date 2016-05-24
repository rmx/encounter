/// <reference path="../data.ts" />
/// <reference path="../Path.ts" />

module rmx.Core {

    // Validation
    // -----------------------------------------------------------------------
    //
    // The result of a validation run on an object.

    export class Validation {
        issues : Issue[] = [];
    }


    export function
    reportIssue
    ( val   : Validation
    , title : string
    , path  : Path
    ): void {
        val.issues.push(new Issue(title, path));
    }


    export class Issue {
        constructor
          ( public title : string
          , public path  : rmx.Path
          ) {}
    }



    export function
    validateEncounter
    ( encounter : rmx.data.Object<rmx.Storage.Encounter>
    ): Validation {
        var val = new Validation();


        if (!encounter.content.name) {
            reportIssue
                ( val
                , 'Name missing'
                , new rmx.Path.Object(encounter.objectId)
                );
        }

        encounter.content.parties.forEach(party => {
            if (party.classes.length === 0) {
                reportIssue
                    ( val
                    , 'Party ' + party.id + ' has no classes'
                    , new rmx.Path.EncounterParty(encounter.objectId, party.id)
                    );
            }

            if (party.objectives.length === 0) {
                reportIssue
                    ( val
                    , 'Party ' + party.id + ' has no objectives'
                    , new rmx.Path.EncounterParty(encounter.objectId, party.id)
                    );
            }

            var spawnPoint = rmx.data.resolveReferenceString<rmx.Storage.TerrainInstance>(party.spawnPoint.terrainEntityId).get(null);
            if (spawnPoint === null) {
                reportIssue
                    ( val
                    , 'Party ' + party.id + ' has no spawn point'
                    , new rmx.Path.EncounterParty(encounter.objectId, party.id)
                    );

            } else {
                var terrain = rmx.data.resolveReference<rmx.Storage.Resource<rmx.Storage.Terrain>>(spawnPoint.terrain).get(null);
                if  (!terrain) {
                    reportIssue
                        ( val
                        , 'Party ' + party.id + ' spawn point references an invalid terrain'
                        , new rmx.Path.EncounterParty(encounter.objectId, party.id)
                        );

                } else {
                    // XXX: The <any> cast
                    var pois = terrain.content.pointsOfInterest.filter(poi => {
                        return poi.name === party.spawnPoint.pointOfInterestName;
                    });

                    if (pois.length === 0) {
                        reportIssue
                            ( val
                            , 'Party ' + party.id + ' spawn point references an invalid point of interest'
                            , new rmx.Path.EncounterParty(encounter.objectId, party.id)
                            );
                    }
                }
            }
        });

        encounter.content.resources.forEach(res => {
            if (res.content instanceof rmx.Storage.Spell) {
                validateSpell(val, encounter, res);

            } else if (res.content instanceof rmx.Storage.Aura) {
                validateAura(val, encounter, res);

            } else if (res.content instanceof rmx.Storage.Creature) {
                validateCreature(val, encounter, res);

            } else if (res.content instanceof rmx.Storage.Class) {
                validateClass(val, encounter, res);

            } else if (res.content instanceof rmx.Storage.Terrain) {
                validateTerrain(val, encounter, res);
            }
        });


        return val;
    }


    function
    validateSpell
    ( val       : Validation
    , encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Spell>
    ): void {
        var spell = resource.content;

        if (!spell.iconId) {
            reportIssue
                ( val
                , 'Spell ' + resource.id + ' has no icon'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }

        if (spell.effects.length === 0) {
            reportIssue
                ( val
                , 'Spell ' + resource.id + ' has no effects'
                , new rmx.Path.EncounterSpellEffects(encounter.objectId, resource.id)
                );
        }

        if (spell.projectile.enabled) {
            if (!spell.sensousEffects.projectile.particleId) {
                reportIssue
                    ( val
                    , 'Spell ' + resource.id + ' uses a projectile but has no particle effect for it'
                    , new rmx.Path.EncounterSpellVisual(encounter.objectId, resource.id)
                    );
            }
        }


        spell.effects.forEach(spellEffect => {
            // XXX tslint: duplicate variable
            var aura;

            if (spellEffect.effect instanceof rmx.Storage.ApplyAura) {
                aura = rmx.data.resolveReference(spellEffect.effect.aura).get(null);
                if (aura === null) {
                    reportIssue
                        ( val
                        , 'Spell ' + resource.id + ' ApplyAura effect references not a valid aura'
                        , new rmx.Path.EncounterSpellEffects(encounter.objectId, resource.id)
                        );
                }

            } else if (spellEffect.effect instanceof rmx.Storage.GroundArea) {
                aura = rmx.data.resolveReference(spellEffect.effect.aura).get(null);
                if (aura === null) {
                    reportIssue
                        ( val
                        , 'Spell ' + resource.id + ' GroundArea effect references not a valid aura'
                        , new rmx.Path.EncounterSpellEffects(encounter.objectId, resource.id)
                        );
                }
            }
        });
    }



    function
    validateAura
    ( val       : Validation
    , encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Aura>
    ): void {
        var aura = resource.content;

        if (!aura.iconId) {
            reportIssue
                ( val
                , 'Aura ' + resource.id + ' has no icon'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }

        if (aura.effects.length === 0) {
            reportIssue
                ( val
                , 'Aura ' + resource.id + ' has no effects'
                , new rmx.Path.EncounterSpellEffects(encounter.objectId, resource.id)
                );
        }
    }



    function
    validateCreature
    ( val       : Validation
    , encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Creature>
    ): void {
        var creature = resource.content;


        var model = rmx.data.resolveReference(creature.model).get(null);
        if (model === null) {
            reportIssue
                ( val
                , 'Creature ' + resource.id + ' has no model'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }


        var behavior = rmx.data.resolveReference(creature.behavior).get(null);
        if (behavior === null) {
            reportIssue
                ( val
                , 'Creature ' + resource.id + ' has no behavior'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }


        if (creature.spells.length === 0) {
            reportIssue
                ( val
                , 'Creature ' + resource.id + ' has no spells'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }
    }



    function
    validateClass
    ( val       : Validation
    , encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Class>
    ): void {
        var creature = resource.content.creature;


        var model = rmx.data.resolveReference(creature.model).get(null);
        if (model === null) {
            reportIssue
                ( val
                , 'Class ' + resource.id + ' has no model'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }


        if (creature.spells.length === 0) {
            reportIssue
                ( val
                , 'Class ' + resource.id + ' has no spells'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }
    }



    function
    validateTerrain
    ( val       : Validation
    , encounter : rmx.data.Object<rmx.Storage.Encounter>
    , resource  : rmx.Storage.Resource<rmx.Storage.Terrain>
    ): void {
        var terrain = resource.content;


        var skybox = rmx.data.resolveReference(terrain.skybox).get(null);
        if (skybox === null) {
            reportIssue
                ( val
                , 'Terrain ' + resource.id + ' has no skybox'
                , new rmx.Path.EncounterResource(encounter.objectId, resource.id)
                );
        }
    }
}
