/// <reference path="./ParticleEffectZones.ts" />
/// <reference path="./ParticleEffect.ts" />
/// <reference path="./RelativePosition.ts" />

/// <reference path="../../ext/three.d.ts" />
declare var TWEEN : any;

module rmx.WebGL {


    // ParticleInitializer
    // -----------------------------------------------------------------------
    //
    // Must implement the 'Initializer' interface!

    export type ParticleInitializer
        = Position
        | Target
        | Lifetime
        | Velocity
        | RadialVelocity
        ;

    /* This doens't work because the fromJSON functions take an additional
     * parameter :(

    var particleInitializerTypes =
        { lifetime : Lifetime
        , Velocity : Velocity
        };

    export function
    parseParticleInitializer(json): ParticleInitializer {
        var type = Object.keys(json)[0]
          , tmp  = { type: type, content: json[type] };

        return rmx.Pure.variantFromJSON(tmp, particleInitializerTypes);
    }
    */


    export function
    parseParticleInitializer(json, nem: NameEntityMap): ParticleInitializer {
        var type = Object.keys(json)[0];

        switch (type) {
        case 'lifetime':        return Lifetime.fromJSON(json[type]);
        case 'velocity':        return Velocity.fromJSON(json[type], nem);
        case 'radialvelocity':  return RadialVelocity.fromJSON(json[type], nem);
        }

        // XXX: Make sure the caller handles exceptions.
        throw new Error('Unknown particle initializer: ' + type);
    }



    export interface Initializer {
        initialize(particle: Particle): void;
    }



    // ParticleModifier
    // -----------------------------------------------------------------------

    export type ParticleModifier
        = Age
        | Accelerate
        | AccelerateFactor
        | Move
        | RandomDrift
        | ColorShift
        | Resize
        | DeathZone
        ;


    export function
    parseParticleModifier(json): ParticleModifier {
        var type = Object.keys(json)[0];

        switch (type) {
        case 'age':               return Age.fromJSON(json[type]);
        case 'accelerate':        return Accelerate.fromJSON(json[type]);
        case 'acceleratefactor':  return AccelerateFactor.fromJSON(json[type]);
        case 'move':              return Move.fromJSON(json[type]);
        case 'randomdrift':       return RandomDrift.fromJSON(json[type]);
        case 'colorshift':        return ColorShift.fromJSON(json[type]);
        case 'resize':            return Resize.fromJSON(json[type]);
        case 'deathzone':         return DeathZone.fromJSON(json[type]);
        }

        // XXX: Make sure the caller handles exceptions.
        throw new Error('Unknown particle initializer: ' + type);
    }

    export interface Modifier {
        update(particle: Particle, dt: number): void;
    }


    export class Position implements Initializer {

        private zone : Zone;

        constructor(zone: Zone) {
            this.zone = zone;
        }

        initialize(particle: Particle): void {
            var pos = this.zone.getLocation();
            particle.position.set(pos[0], pos[1], pos[2]);
        }
    }

    export class Velocity implements Initializer {

        private zone : Zone;

        static fromJSON(args: any, objects: any) {
            var zone = ZoneFactory(args, objects);
            return new Velocity(zone);
        }

        constructor(zone: Zone) {
            this.zone = zone;
        }

        initialize(particle: Particle): void {
            var pos = this.zone.getLocation();
            particle.velocity.set(pos[0], pos[1], pos[2]);

            //FIXME:
            VectorPool.release(pos);
        }
    }


    export class RadialVelocity implements Initializer {

        private velocity: Velocity;

        static fromJSON(args: any, objects: any) {
            var pos    = new RelativePosition(args[0], objects);
            var norm   = vec3.fromValues(0, 0, 1);
            var offset = vec3.fromValues(0, 0, 0);
            var zone   = new DiscZone(pos, offset, norm, args[1], args[2]);
            return new RadialVelocity(new Velocity(zone));
        }

        constructor(velocity: Velocity) {
            this.velocity = velocity;
        }

        initialize(particle: Particle): void {
            this.velocity.initialize(particle);
        }
    }


    export class Target implements Initializer {

        private target : ParticleId;
        private color  : number[];
        private size   : number[];
        private effect : any;

        constructor(target: ParticleId, color: number[],
                    size: number[], effect:any) {
            this.target = target;
            this.color  = color.slice(0);
            this.size   = size.slice(0);
            this.effect = effect;
        }

        initialize(particle: Particle): void {
            if (this.effect) {
                particle.target =
                    this.effect.setTargetParticle(this.color, this.size);
            } else {
                particle.target = this.target;
            }

            particle.isDead = false;

            particle.size  = this.size.slice(0);
            particle.color = this.color.slice(0);

            particle.origSize  = this.size.slice(0);
            particle.origColor = this.color.slice(0);
        }
    }


    export class Lifetime implements Initializer {

        private min : number;
        private max : number;

        static fromJSON(args: any) {
            return new Lifetime(args[0], args[1]);
        }

        constructor(min: number, max: number) {
            var _ref;
            this.min = min;
            this.max = max;
            this.max = (_ref = this.max) != null ? _ref : this.min;
        }

        initialize(particle: Particle): void {
            particle.lifetime =
                this.min + Math.random() * (this.max - this.min);
        }
    }


    export class Age implements Modifier {

        private easing : any;

        static fromJSON(args: any) {
            return new Age(args[0]);
        }

        constructor(easing: Object) {
            this.easing = easing != null ? easing :
                            TWEEN.Easing.Linear.None;
        }

        update(particle: Particle, time: number): void {
            particle.age += time;
            if (particle.age >= particle.lifetime) {
                particle.energy = 0;
                particle.isDead = true;
            } else {
                var t = this.easing(particle.age / particle.lifetime);
                particle.energy = -1 * t + 1;
            }
        }
    }

    export class Move implements Modifier {

        private velocityVerlet : boolean;

        static fromJSON(args: any) {
            return new Move();
        }

        constructor() {
            this.velocityVerlet = false;
        }

        update(particle: Particle, time: number): void {
            var p = particle.position;
            var v = particle.velocity;
            var old = particle._oldvelocity;
            if (this.velocityVerlet) {
                p.x += (v.x + old.x) * 0.5 * time;
                p.y += (v.y + old.y) * 0.5 * time;
                p.z += (v.z + old.z) * 0.5 * time;
            } else {
                p.x += v.x * time;
                p.y += v.y * time;
                p.z += v.z * time;
            }
        }
    }


    export class Accelerate implements Modifier {

        private acceleration : any;

        static fromJSON(args: any) {
            var velocity = new THREE.Vector3(args[0], args[1], args[2]);
            return new Accelerate(velocity);
        }

        constructor(acceleration: any) {
            this.acceleration = acceleration;
        }

        update(particle: Particle, time: number) {
            particle._oldvelocity.copy(particle.velocity);
            return particle.velocity.add(
                    (this.acceleration.clone()).multiplyScalar(time));
        }
    }


    export class AccelerateFactor implements Modifier {

        private factor : number;

        static fromJSON(args: any) {
            return new AccelerateFactor(args[0]);
        }

        constructor(factor: number) {
            this.factor = factor;
        }

        update(particle: Particle, time: number) {
            var abs_vel = particle.velocity.length();
            if (abs_vel === 0)
                return;
            var adjFactor = this.factor * time / abs_vel + 1;
            return particle.velocity.multiplyScalar(adjFactor);
        }
    }


    export class AccelerateVelocity {

        private factor : number;

        static fromJSON(args: any) {
            return new AccelerateVelocity(args[0]);
        }

        constructor(factor) {
            this.factor = factor;
        }

        update(emitter: any, particle: Particle, time: number) {
            var v = particle.velocity;
            v.z  += -v.x * this.factor;
            v.y  += v.z * this.factor;
            return v.x += v.y * this.factor;
        }
    }


    export class RandomDrift implements Modifier {

        private drift : THREE.Vector3;

        static fromJSON(args: any) {
            var drift = new THREE.Vector3(args[0], args[1], args[2]);
            return new RandomDrift(drift);
        }

        constructor(drift: THREE.Vector3) {
            this.drift = drift;
        }

        update(particle: Particle, time: number): void {
            particle.velocity.x += (Math.random() - 0.5) * this.drift.x * time;
            particle.velocity.y += (Math.random() - 0.5) * this.drift.y * time;
            particle.velocity.z += (Math.random() - 0.5) * this.drift.z * time;
        }
    }

    export class ColorShift implements Modifier {

        private target : number[];
        private easing : any;

        static fromJSON(args: any) {
            return new ColorShift(args[0], args[1], args[2], args[3]);
        }

        constructor(target_hue: number, target_sat: number,
                    target_val: number, easing: any) {
            this.target = [100, 100, 100];
            this.target[0] = target_hue / 360;
            this.target[1] = target_sat;
            this.target[2] = target_val;
            this.easing = easing != null ? easing : TWEEN.Easing.Linear.None;
        }

        update(particle: Particle, time: number): void {
            var delta = -1 * particle.energy + 1;
            particle.color[0] = particle.origColor[0] +
                (this.target[0] - particle.origColor[0]) * delta;
            particle.color[1] = particle.origColor[1] +
                (this.target[1] - particle.origColor[1]) * delta;
            particle.color[2] = particle.origColor[2] +
                (this.target[2] - particle.origColor[2]) * delta;
        }
    }


    export class Resize implements Modifier {

        private target : number[];
        private easing : any;

        static fromJSON(args: any) {
            return new Resize(args[0], args[1], args[2]);
        }

        constructor(target_min_size: number, target_max_size: number,
                    easing: Object) {
            this.target = [target_min_size, target_max_size];
            this.easing = easing != null ? easing : TWEEN.Easing.Linear.None;
        }

        update(particle: Particle, time: number): void {
            var delta = -1 * particle.energy + 1;
            particle.size[0] = particle.origSize[0] +
                (this.target[0] - particle.origSize[0]) * delta;
            particle.size[1] = particle.origSize[1] +
                (this.target[1] - particle.origSize[1]) * delta;
        }
    }


    export class DeathZone {

        private zone : Zone;

        static fromJSON(args: any) {
            var zone = ZoneFactory(args, null);
            return new DeathZone(zone);
        }

        constructor(zone: Zone) {
            this.zone = zone;
        }

        update(particle: Particle, time: number): void {
            var relParticlePos = RelativePosition.fromVector(particle.position);
            if (this.zone.contains(relParticlePos)) {
                particle.isDead = true;
            }
        }
    }
}
