/// <reference path="./Utils.ts" />
/// <reference path="./ParticleEffectZones.ts" />
/// <reference path="./ParticleEffectModifiers.ts" />

/// <reference path="../time.ts" />
/// <reference path="../../ext/three.d.ts" />

interface ParticleId extends Number {}

interface ParticleEffectSourceDefinition {

    counter  : String;
    blending : string;
    npart    : number;
    offset   : number[];
    color    : number[];
    size     : number[];
    delay    : number;
    zone     : any;
}

module rmx.WebGL {

    export class Particle {
        isDead       : boolean;
        lifetime     : number;
        age          : number;
        energy       : number;
        size         : number[];
        color        : number[];
        origSize     : number[];
        origColor    : number[];
        target       : ParticleId;
        position     : THREE.Vector3;
        velocity     : THREE.Vector3;
        _oldvelocity : THREE.Vector3;

        constructor() {
            this.lifetime  = 0;
            this.age       = 0;
            this.energy    = 1;
            this.size      = [1.0, 1.0];
            this.color     = [100.0, 1.0, 1.0];
            this.origSize  = [1.0, 1.0];
            this.origColor = [100.0, 1.0, 1.0];
            this.isDead    = true;
            this.target    = null;
            this.position  = THREEVectorPool.get().set(0, 0, 0);
            this.velocity  = THREEVectorPool.get().set(0, 0, 0);
            this._oldvelocity = THREEVectorPool.get().set(0, 0, 0);
        }

        release(): void {
            if (this.color != null) {
                delete this.color;
            }
            if (this.size != null) {
                delete this.size;
            }
        }

        reset(): Particle {
            this.lifetime  = 0;
            this.age       = 0;
            this.energy    = 1;
            this.isDead    = true;
            this.target    = null;
            this.size      = [1.0, 1.0];
            this.color     = [100.0, 1.0, 1.0];
            this.origSize  = [1.0, 1.0];
            this.origColor = [100.0, 1.0, 1.0];
            this.position.set(0, 0, 0);
            this.velocity.set(0, 0, 0);
            this._oldvelocity.set(0, 0, 0);
            return this;
        }
    }


    export class ParticlePool {

        private static pool : Particle[] = [];

        static releaseAll(): void {
            this.pool = [];
        }

        static get(): Particle {
            if (this.pool.length > 0) {
                return this.pool.pop().reset();
            } else {
                return new Particle();
            }
        }

        static release(particle: Particle): void {
            particle.release();
            this.pool.push(particle);
        }
    }


    export interface Counter {
        updateEmitter(dt: number): number;
    }


    export class SteadyCounter {

        // number of particles per second
        rate     : number;
        leftover : number;

        constructor(rate: number) {
            this.rate = rate;
            this.leftover = 0;
        }

        updateEmitter(dt: number): number {
            var targetRelease = dt * this.rate + this.leftover
              , actualRelease = Math.floor(targetRelease);

            this.leftover = targetRelease - actualRelease;

            return actualRelease;
        }
    }


    export class ShotCounter {

        private used         : boolean;
        private numParticles : number;

        constructor(numParticles: number) {
            this.used = false;
            this.numParticles = numParticles;
        }

        updateEmitter(dt: number): number {
            if (this.used) {
                return 0;
            } else {
                this.used = true;
                return this.numParticles;
            }
        }
    }


    export class ParticleEffect {

        size     : number[];
        npart    : number;
        color    : number[];
        offset   : number[];
        counter  : String;
        blending : THREE.Blending;
        delay    : number;

        // parse json initializers and modifiers to initializers
        // and zones
        initializers : ParticleInitializer[];
        modifiers    : ParticleModifier[];

        // emitter definition
        sourceZone  : any;

        // debugger helper
        creationTime : number;

        private objects : NameEntityMap;

        static fromJSON(
            effect_definition: rmx.Storage.ParticleEffect,
            objects: NameEntityMap): ParticleEffect {

            var json = JSON.parse(effect_definition.script);
            var pe = new ParticleEffect(json.source, objects);

            // instanciate and add all initializers and modifiers
            json.initializers.forEach(x => {
                pe.initializers.push(parseParticleInitializer(x, objects));
            });

            json.modifiers.forEach(x => {
                pe.modifiers.push(parseParticleModifier(x));
            });

            return pe;
        }

        constructor(effect_source: ParticleEffectSourceDefinition,
                    objects: NameEntityMap) {

            this.objects      = objects;
            this.modifiers    = [];
            this.initializers = [];

            // parse various values and provide sane defaults
            this.counter  = effect_source.counter  || "shot";
            this.blending = this.parseBlending(effect_source.blending);

            this.npart  = effect_source.npart;
            this.offset = effect_source.offset;
            this.size   = effect_source.size;
            this.delay  = effect_source.delay || 0.0;

            this.color     = effect_source.color;
            this.color[0] /= 360;

            // finally create the emitter zone
            this.setEmitterZone(effect_source.zone, this.offset);

            // keep creation time for debugging purposes
            this.creationTime = rmx.now();
        }

        private parseBlending(blending: string): THREE.Blending {

            switch(blending) {
                case "no":          return THREE.NoBlending;
                case "normal":      return THREE.NormalBlending;
                case "additive":    return THREE.AdditiveBlending;
                case "subtractive": return THREE.SubtractiveBlending;
                case "multiply":    return THREE.MultiplyBlending;
                case "custom":      return THREE.CustomBlending;
                default:            return THREE.CustomBlending;
            }
        }

        private setEmitterZone(zone : Zone, offset : number[]): void {
            var offset_ : Vec3;
            if (offset != null) {
                offset_ = vec3.fromValues(offset[0], offset[1], offset[2]);
            } else {
                offset_ = vec3.fromValues(0.0, 0.0, 0.0);
            }

            this.sourceZone = ZoneFactory(zone, this.objects, offset_, null);
            if (this.sourceZone == null) {
                throw Error("Unkown source zone: " + zone + "!");
            }
        }
    }
}
