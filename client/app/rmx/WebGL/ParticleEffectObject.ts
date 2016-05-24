/// <reference path="../../ext/three.d.ts" />

/// <reference path="../Storage/Types.ts" />
/// <reference path="./NameEntityMap.ts" />

module rmx.WebGL {

    export class ParticleEffectObject {

        generationNumber : number;
        particleEffect   : rmx.Storage.ParticleEffect;
        nem              : NameEntityMap;
        oneshot          : boolean;

        constructor(
            public id: string, pe: rmx.Storage.ParticleEffect,
            nem: NameEntityMap) {

            this.generationNumber = null;
            this.particleEffect   = pe;
            this.nem              = nem;
        }
    }
}
