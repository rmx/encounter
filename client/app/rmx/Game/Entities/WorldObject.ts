/// <reference path="../../../ext/gl-matrix.ts" />

/// <reference path="../../Pure/Ids.ts" />
/// <reference path="../../Pure/Terrain.ts" />
/// <reference path="../../Pure/MovePath.ts" />
/// <reference path="../../Pure/Game.ts" />
/// <reference path="../../Pure/JSON.ts" />

/// <reference path="../Types.ts" />


module rmx.Game {

    import EntityId            = rmx.Pure.EntityId;
    import Aura                = rmx.Pure.Aura;
    import CooldownTimer       = rmx.Pure.CooldownTimer;
    import MovePath            = rmx.Pure.MovePath;
    import TerrainPosition     = rmx.Pure.TerrainPosition;
    import Elevation           = rmx.Pure.Elevation;
    import ElevationTransition = rmx.Pure.ElevationTransition;

    export enum AnimationDeck {Submerged, Ground, Floating, Air };

    function updateField(obj: any, json: any, name: string): void {
        var value = json[name];
        if (value != undefined) {
            obj[name] = value;
        }
    }

    function updateEnumField(obj: any, json: any, enum_t: any, name: string): void {
        var value = json[name];
        if (value != undefined)
            obj[name] = rmx.Pure.JSON.enumFromJSON(enum_t, value);
    }

    export class WorldObject implements rmx.Pure.IEntity {

        id              : EntityId;

        name            : string;

        soul            : string; // present | fading

        health          : number;
        maxHealth       : number;

        movementSpeed   : number;

        scale           : number;
        targetId        : EntityId;
        faction         : string;

        activePowerType : string;
        powers          : Map<string, any>;

        stunned         : boolean;
        moveFlags       : number;
        terrainPosition : TerrainPosition;
        position        : Vec3;   // last projected terrain position

        spell           : rmx.Pure.Spell;

        appearance      : any;

        positionType     : AnimationDeck;
        prevPositionType : AnimationDeck;

        animationLockUntil  : number;
        transitionAnimation : string;

        // Ideally this would be a Map<string, Aura>. But since Chrome still
        // doesn't implement iterators, this has to be something which we can
        // iterate over, such as a ordinary Object.
        auras           : { [slot: string]: Aura };

        movePath        : MovePath;


        // This is NOT the same property as on the server. It contains the
        // consolidated cooldowns for each spell. The client currently doesn't
        // have enough info to compute the cooldowns locally.
        spellCooldowns  : Map<string, CooldownTimer>;


        elevation           : Elevation;
        elevationTransition : ElevationTransition;


        constructor(id: string) {
            this.id             = id;
            this.powers         = new Map<string, any>();
            this.auras          = Object.create(null);
            this.spellCooldowns = new Map<string, CooldownTimer>();
            this.stunned        = false;

            this.prevPositionType = null;
        }

        updateFields(json): void {
            updateField(this, json, 'name');

            updateField(this, json, 'soul');

            updateField(this, json, 'health');
            updateField(this, json, 'maxHealth');

            updateField(this, json, 'movementSpeed');

            updateField(this, json, 'scale');
            updateField(this, json, 'targetId');
            updateField(this, json, 'faction');

            updateField(this, json, 'activePowerType');

            updateField(this, json, 'moveFlags');

            updateField(this, json, 'stunned');

            updateField(this, json, 'appearance');

            updateEnumField(this, json, AnimationDeck, 'positionType');

            updateEnumField(this, json, Elevation, 'elevation');
            if (json.elevationTransition) {
                this.elevationTransition = ElevationTransition.parseJSON(json.elevationTransition);
            }
        }

        static parseJSON(json: any): WorldObject {
            var entity = new WorldObject(json.id);
            entity.terrainPosition = TerrainPosition.parseJSON(json.position);
            entity.updateFields(json.fields);
            return entity;
        }

        get healthPercent(): number {
            return 100 * this.health / this.maxHealth;
        }

        get power(): number {
            return this.powers.get(this.activePowerType).value;
        }

        get maxPower(): number {
            return this.powers.get(this.activePowerType).maxValue;
        }

        get powerPercent(): number {
            return 100 * this.power / this.maxPower;
        }

        get auraList(): Aura[] {
            return _.values(this.auras);
        }

        get animationDeckPrefix(): string {
            switch(this.positionType) {
                case AnimationDeck.Ground: return "";
                default: return this.animationDeckName(this.positionType) + "_";
            }
        }

        get animationTransitionLabel(): string {

           return this.animationDeckName(this.prevPositionType) + "_to_" +
                  this.animationDeckName(this.positionType);
        }

        private animationDeckName(posType: AnimationDeck): string {
            switch(posType) {
                case AnimationDeck.Air: return "air";
                case AnimationDeck.Ground: return "ground";
                case AnimationDeck.Floating: return "floating";
                case AnimationDeck.Submerged: return "submerged";
                default: return "";
            }
        }
    }
}
