/// <reference path="./File.ts" />
/// <reference path="./Skin.ts" />

module rmx.Storage {

    // -----------------------------------------------------------------------
    export class AnimationLabel {

        name  : string;
        start : number;
        end   : number;
        fps   : number;

        static mk(name: string): AnimationLabel {
            return Avers.mk<AnimationLabel>(AnimationLabel,
                { name : name
                }
            );
        }
    }

    Avers.definePrimitive(AnimationLabel, 'name');
    Avers.definePrimitive(AnimationLabel, 'start', 0);
    Avers.definePrimitive(AnimationLabel, 'end',   100);
    Avers.definePrimitive(AnimationLabel, 'fps',   25);



    // -----------------------------------------------------------------------
    export class Model {

        name            : string;
        files           : File[];
        skins           : Skin[];
        animationLabels : AnimationLabel[];
        vertexTags      : any;

        static mk(name: string): Model {
            return Avers.mk<Model>(Model,
                { name : name
                }
            );
        }
    }

    Avers.definePrimitive  (Model, 'name', '');
    Avers.defineCollection (Model, 'files', File);
    Avers.defineCollection (Model, 'skins', Skin);
    Avers.defineCollection (Model, 'animationLabels', AnimationLabel);
}
