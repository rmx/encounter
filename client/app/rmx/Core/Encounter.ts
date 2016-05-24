/// <reference path="../Storage/Types.ts" />
/// <reference path="../data.ts" />

module rmx.Core {

    export function
    encounterName(encounterId: string): string {
        return rmx.data.findById<rmx.Storage.Encounter>(encounterId).fmap(entity => {
            return entity.content.name;
        }).get(encounterId);
    }


    export function
    resourceSearchResult(encounter: rmx.data.Object<Storage.Encounter>, filter): Computation<any[]> {
        return new Computation(() => {
            return encounter.content.resources.filter(res => {
                return filter(res);
            }).map(res => {
                return rmx.Storage.Reference.mk(encounter.objectId, null, 'resources.' + res.id);
            });
        });
    }

    export function
    auraSearchResult(encounter: rmx.data.Object<Storage.Encounter>): Computation<any[]> {
        return resourceSearchResult(encounter, res => {
            return res.type === 'aura';
        });
    }


    export function
    behaviorSearchResult(encounter: rmx.data.Object<Storage.Encounter>): Computation<any[]> {
        return resourceSearchResult(encounter, res => {
            return res.type === 'behavior';
        });
    }

    export function
    terrainSearchResult(encounter: rmx.data.Object<Storage.Encounter>): Computation<any[]> {
        return resourceSearchResult(encounter, res => {
            return res.type === 'terrain';
        });
    }

    export function
    auraDescription(aura: rmx.Storage.Aura): string {

        var uniqnessValue = "";
        switch(aura.uniquenessConstraint.condition) {
            case 'target-category':
                uniqnessValue = 'Flask or total damage immunity';
                break;
            case 'caster-target-category':
                uniqnessValue = 'Potion or damage absorbing shield';
                break;
            case 'local-stacking':
                uniqnessValue = "Orb";
                break;
        }

        uniqnessValue = uniqnessValue + ". ";

        var durationValue = [aura.duration.content.min, ' - ',
                             aura.duration.content.max].join("");
        if(aura.duration.content.min == aura.duration.content.max)
            durationValue = "" + aura.duration.content.min;
        durationValue = durationValue + " sec. ";


        return [
              uniqnessValue
            , durationValue
            , aura.description
            , '.'
        ].join("");
    }
}
