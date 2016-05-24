module rmx {

    export type Path
        = Path.Index
        | Path.Object
        | Path.EncounterParty
        | Path.EncounterResource
        | Path.EncounterSpellEffects
        | Path.EncounterSpellVisual
        ;



    // parsePathString
    // -----------------------------------------------------------------------
    //
    // This function is asynchronous because parsing the string may depend on
    // the type of the object the path is referring to. So we return
    // a Promise.

    export function
    parsePathString(path: string): Promise<Path> {
        return new Promise((resolve, reject) => {
            reject(new Error("..."));
        });
    }



    export module Path {


        export interface IPath {
            toPath(): string;
        }


        function concatElements(...xs: string[]): string {
            return '/' + xs.join('/');
        }



        export class Index implements IPath {
            toPath() {
                return '/';
            }
        }



        export class Object implements IPath {

            constructor
              ( public objectId : string
              ) {}

            toPath() {
                return concatElements('o', this.objectId);
            }
        }



        export class EncounterParty implements IPath {

            constructor
              ( public encounterId : string
              , public partyId     : string
              ) {}

            toPath() {
                return concatElements
                    ( 'o'
                    , this.encounterId
                    , 'parties'
                    , this.partyId
                    );
            }
        }



        export class EncounterResource implements IPath {

            constructor
              ( public encounterId : string
              , public resourceId  : string
              ) {}

            toPath() {
                return concatElements
                    ( 'o'
                    , this.encounterId
                    , 'resources'
                    , this.resourceId
                    );
            }
        }



        export class EncounterSpellEffects implements IPath {

            constructor
              ( public encounterId : string
              , public resourceId  : string
              ) {}

            toPath() {
                return concatElements
                    ( 'o'
                    , this.encounterId
                    , 'resources'
                    , this.resourceId
                    , 'effects'
                    );
            }
        }



        export class EncounterSpellVisual implements IPath {

            constructor
              ( public encounterId : string
              , public resourceId  : string
              ) {}

            toPath() {
                return concatElements
                    ( 'o'
                    , this.encounterId
                    , 'resources'
                    , this.resourceId
                    , 'visual'
                    );
            }
        }
    }
}
