module rmx.Storage {

    export class ParticleEffect {

        id     : string;
        name   : string;
        script : string;

        constructor() {
            Avers.initializeProperties(this);
        }

        static mk(name: string) {
            return Avers.mk(ParticleEffect, { name: name, script: "{}" });
        }

    }

    Avers.definePrimitive(ParticleEffect, 'name', 'NewParticleEffect');
    Avers.definePrimitive(ParticleEffect, 'script', '{}');
}
