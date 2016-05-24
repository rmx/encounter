/// <reference path="./rmx/WebGL/NameEntityMap.ts" />
/// <reference path="./rmx/WebGL/Utils.ts" />
/// <reference path="./rmx/WebGL/ParticleEffect.ts" />
/// <reference path="./rmx/WebGL/ParticleEffectModifiers.ts" />
/// <reference path="./rmx/WebGL/ParticleEffectZones.ts" />

module rmx.WebGL {

    module ParticleShaderMaterialResources {

        var texture : THREE.Texture = null;

        export var vShader = [
                "attribute float size;",
                "attribute vec3 pcolor;",
                "varying vec3 vColor;",
                "void main() {",
                    "vColor = pcolor;",
                    "vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);",
                    "gl_PointSize = size * (200.0 / length(mvPosition.xyz));",
                    "gl_Position  = projectionMatrix * mvPosition;",
                "}"].join("");

        export var fShader = [
                "uniform sampler2D texture;",
                "varying vec3 vColor;",
                "void main() {",
                    "gl_FragColor = vec4(vColor, 1.0);",
                    "gl_FragColor = gl_FragColor *",
                                   "texture2D(texture, gl_PointCoord);",
                "}"].join("");

        export function uniforms() {
            return { texture: { type  : "t", value : getTexture() } };
        }

        function getTexture() : THREE.Texture {

            if(texture != null)
                return texture;

            var canvas = document.createElement('canvas');
            canvas.width = canvas.height = 128;

            var context = canvas.getContext('2d');
            context.beginPath();
            context.arc(64, 64, 60, 0, Math.PI * 2, false);
            context.closePath();

            var s = canvas.width / 2;
            var gradient = context.createRadialGradient(s, s, 0, s, s, s);
            gradient.addColorStop(0.0, 'rgba(255,255,255,0.5)');
            gradient.addColorStop(0.2, 'rgba(255,255,255,0.4)');
            gradient.addColorStop(0.4, 'rgba(255,255,255,0.3)');
            gradient.addColorStop(1.0, 'rgba(255,255,255,0.0)');

            context.fillStyle = gradient;
            context.fill();

            texture = new THREE.Texture(canvas);
            texture.needsUpdate = true;
            return texture;
        }
    }

    export interface ParticleRenderer {
        stop(): void;
        step(time: number, elapsed: number): void;
        getThreeObject()   : THREE.PointCloud;
        hasFinished(): boolean;
    }


    export class NullParticleRenderer implements ParticleRenderer {
        stop(): void {
        }

        step(time: number, elapsed: number): void {
        }

        getThreeObject() : THREE.PointCloud {
            return <THREE.PointCloud>new THREE.Object3D();
        }

        hasFinished(): boolean {
            return true;
        }
    }


    // A particle engine that uses textures to render and store particles.
    export class ShaderParticleRenderer implements ParticleRenderer {
        stop(): void {
        }

        step(time: number, elapsed: number): void {
        }

        getThreeObject() : THREE.PointCloud {
            return <THREE.PointCloud>new THREE.Object3D();
        }

        hasFinished(): boolean {
            return true;
        }
    }


    // A simple software particle renderer.
    export class SoftwareParticleRenderer implements ParticleRenderer {

        private velocityVerlet : boolean;

        private counter        : Counter;

        private TIMESTEP       : number;
        private freeIDs        : number[];
        private particles      : rmx.WebGL.Particle[];

        private initializers   : ParticleInitializer[];
        private actions        : ParticleModifier[];

        private particles_rep  : THREE.Geometry;

        private delay          : number;

        // TODO: Look if three.d.ts doesn't have a proper type for this, if
        // not then define one in this file.
        private attributes     : any;

        private particleSystemRep    : THREE.PointCloud;
        private activeParticles : number;

        constructor(effect: rmx.WebGL.ParticleEffect) {

            switch (effect.counter) {
            case "steady":
                this.counter = new
                        rmx.WebGL.SteadyCounter(effect.npart);
                break;
            case "shot":
                this.counter = new
                    rmx.WebGL.ShotCounter(effect.npart);
                break;
            }

            this.delay         = effect.delay;

            this.particles       = [];
            this.initializers    = [];
            this.actions         = [];
            this.TIMESTEP        = 15 / 1000;
            this.velocityVerlet  = false;
            this.activeParticles = 0;

            // add the emitter for this particle effect
            this.addInitializer(new rmx.WebGL.Position(
                        effect.sourceZone));

            // adding the initializer for freshly emitted particles
            this.addInitializer(new rmx.WebGL.Target(
                null, effect.color, effect.size, this));

            effect.initializers.forEach( x => {
                this.addInitializer(x);
            });

            effect.modifiers.forEach( x => {
                this.addAction(x);
            });

            // particle effects are limited to 7000 living particles
            this.freeIDs = [];
            this.particles_rep = new THREE.Geometry();
            _.times(7000, (i) => {
                this.freeIDs.push(i);
                this.particles_rep.vertices.push(new THREE.Vector3());
            });

            this.attributes = {
                size  : { type: 'f', value: [] },
                pcolor: { type: 'c', value: [] }
            };

            var shaderMaterial = new THREE.ShaderMaterial({
                uniforms       : ParticleShaderMaterialResources.uniforms(),
                vertexShader   : ParticleShaderMaterialResources.vShader,
                fragmentShader : ParticleShaderMaterialResources.fShader,
                depthWrite     : false,
                transparent    : true,
                attributes     : this.attributes,
                blending       : effect.blending
            });

            this.particleSystemRep =
                new THREE.PointCloud(this.particles_rep, shaderMaterial);

            // FIXME: r68 doesn't seem to have this.
            // this.particleSystemRep.dynamic = true;

            this.hideAllParticles();
        }

        getThreeObject(): THREE.PointCloud {
            return this.particleSystemRep;
        }

        hasFinished(): boolean {
            return this.activeParticles == 0;
        }

        stop(): void {
            var i, len;
            for (i = 0, len = this.particles.length; i < len; i++) {
                var particle = this.particles[i];
                this.onParticleDead(particle);
                rmx.WebGL.ParticlePool.release(particle);
            }
            this.particles = [];
        }

        // time is in ms, elapsed in s! (FIXME why?)
        step(time: number, elapsed: number): void {

            if(this.delay > 0) {
                this.delay -= elapsed;
            } else {
                if (!this.velocityVerlet) {
                    var maxBlock = this.TIMESTEP * 20;
                    if (elapsed >= maxBlock) {
                        elapsed = maxBlock;
                    }

                    while (elapsed >= this.TIMESTEP) {
                        this.update(this.TIMESTEP);
                        elapsed -= this.TIMESTEP;
                    }
                } else {
                    this.update(elapsed);
                }
            }
        }

        // elapsed in seconds!
        private update(elapsed: number): void {

            var num_part = this.counter.updateEmitter(elapsed);
            _.times(num_part, () => {
                this.createParticle();
            });

            this.actions.forEach( act => {
                this.particles.forEach( part => {
                    act.update(part, elapsed);

                    //XXX: the modifiers can affect the particle size and
                    //     color so we need to update the shader attributes
                    //     here.
                    this.updateAttributes(part);
                    //var ssize = Math.random() * (part.size[1] - part.size[0]) +
                                //part.size[0];
                    //this.attributes.size.value[id] = ssize;
                    //rmx.WebGL.setHSV(
                        //this.attributes.pcolor.value[id],
                        //part.color[0], part.color[1], part.color[2]);
                });
            });

            for (var i = this.particles.length - 1; i >= 0; i--) {
                var particle = this.particles[i];
                if (particle.isDead) {
                    this.onParticleDead(particle);
                    rmx.WebGL.ParticlePool.release(particle);
                    this.particles.splice(i, 1);
                }
            }

            this.particleSystemRep.geometry.verticesNeedUpdate = true;
            this.attributes.size.needsUpdate   = true;
            this.attributes.pcolor.needsUpdate = true;
        }

        private updateAttributes(part: rmx.WebGL.Particle): void {

            var id = <number>part.target;

            this.attributes.size.value[id] =
                Math.random() * (part.size[1] - part.size[0]) + part.size[0];

            rmx.WebGL.setHSV(this.attributes.pcolor.value[id],
                part.color[0], part.color[1], part.color[2]);
        }

        private createParticle(): void {
            var particle = rmx.WebGL.ParticlePool.get();
            this.initializers.forEach(x => {
                x.initialize(particle);
            });

            this.particles.push(particle);

            // XXX: activeParticles is always particles.length ?
            this.activeParticles += 1;
            var id = <number>particle.target;

            if (id) {
                this.particles_rep.vertices[id] = particle.position;
                this.updateAttributes(particle);
            }
        }

        addInitializer(initializer: rmx.WebGL.ParticleInitializer): void {
            this.initializers.push(initializer);
        }

        addAction(action: rmx.WebGL.ParticleModifier): void {
            this.actions.push(action);
        }

        removeInitializer(
            initializer: rmx.WebGL.ParticleInitializer): void {

            var index = this.initializers.indexOf(initializer);
            if (index > -1)
                this.initializers.splice(index, 1);
        }

        removeAction(action: rmx.WebGL.ParticleModifier): void {
            var index = this.actions.indexOf(action);
            if (index > -1)
                this.actions.splice(index, 1);
        }

        //FIXME: cant we just return a initialized particle?
        setTargetParticle(color: number[], size: number[]): ParticleId {
            var id = null;
            if (this.freeIDs.length > 0) {
                id = this.freeIDs.pop();
                this.attributes.size.value[id] =
                    Math.random() * (size[1] - size[0]) + size[0];
                rmx.WebGL.setHSV(this.attributes.pcolor.value[id],
                       color[0], color[1], color[2]);
            } else {
                console.log("No more free IDs available for this system!");
            }

            return id;
        }

        onParticleDead(particle: rmx.WebGL.Particle): void {
            this.activeParticles -= 1;
            var id = <number>particle.target;
            if (id) {
                this.hideParticle(id);
                this.freeIDs.push(id);
            }
        }

        private hideAllParticles(): void {
            var len = this.particleSystemRep.geometry.vertices.length;
            for (var v = 0; v < len; v++)
                this.hideParticle(v);
        }

        private hideParticle(id: ParticleId): void {
            this.attributes.size.value[<any>id]   = 0.0;
            this.attributes.pcolor.value[<any>id] = new THREE.Color(0xffffff);
            //rmx.WebGL.setHSV(
                //this.attributes.pcolor.value[id], 0, 0, 0);

            this.particles_rep.vertices[<number>id].set(
                    Number.POSITIVE_INFINITY,
                    Number.POSITIVE_INFINITY,
                    Number.POSITIVE_INFINITY);
        }

    }


    export interface ParticleRendererCtor {
        new(effect: rmx.WebGL.ParticleEffect) : ParticleRenderer;
    }

    // engine iterates over all entities (instanceof ParticleEffect) and
    // updates on the representation directly.
    export class ParticleEngine {

        private scene           : THREE.Scene;
        private renderer        : ParticleRendererCtor;

        particleEffects : { [id: string]: ParticleRenderer; };


        constructor(scene   : THREE.Scene,
                    renderer: ParticleRendererCtor = NullParticleRenderer) {

            this.scene    = scene;
            this.renderer = renderer;

            this.particleEffects = {};
        }

        setRenderer(renderer : ParticleRendererCtor) {
            this.renderer = renderer;
        }

        update(time: number, elapsed: number) : void {
            Object.keys(this.particleEffects).forEach(
                (effect) => {
                    this.particleEffects[effect].step(time, elapsed);
            });
        }

        hasFinished(id: string): boolean {
            var pe = this.particleEffects[id];
            if (pe) {
                return pe.hasFinished();
            }

            return false;
        }

        remove(id: string): void {
            var pe = this.particleEffects[id];
            if (pe) {
                pe.stop();
                this.scene.remove(pe.getThreeObject());
                delete this.particleEffects[id];
            }
        }

        addParticleEffect(
            id: string, obj: rmx.Storage.ParticleEffect,
            nameToId: NameEntityMap
            ): void {

            if (Object.keys(this.particleEffects).length >= 32) {
                return;
            }

            var effect = rmx.WebGL.ParticleEffect.fromJSON(
                                obj, nameToId);

            var part_sys = new this.renderer(effect);
            this.scene.add(part_sys.getThreeObject());

            this.particleEffects[id] = part_sys;
        }
    }
}
