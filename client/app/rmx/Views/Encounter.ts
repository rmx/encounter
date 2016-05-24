/// <reference path="../../entry.ts" />
/// <reference path="../Views.ts" />

module rmx.Views.Encounter {

    export function
    mkSidebarItems(encounter: rmx.data.Object<rmx.Storage.Encounter>): ISidebarItem[] {
        var contextPath = '/o/' + encounter.objectId;

        return [ { href:         contextPath + '/'
                 , iconClass:    'icon-id-card'
                 , label:        'Identity'
                 , description:  'Make your encounter appealing to new users.'
                 }
               , { href:         contextPath + '/images'
                 , iconClass:    'icon-pen'
                 , label:        'Images'
                 , description:  'Images which are used on the public encounter page and in other places.'
                 }
               , { href:         contextPath + '/glue'
                 , iconClass:    'icon-cog'
                 , label:        'Glue'
                 , description:  'The main glue script for the encounter.'
                 }
               , { href:         contextPath + '/scoringFunction'
                 , iconClass:    'icon-cog'
                 , label:        'Scoring function'
                 , description:  'Function which computes the score.'
                 }
               , { href:         contextPath + '/resources'
                 , iconClass:    'icon-pen'
                 , label:        'Resources'
                 , description:  'All resources which the encounter uses.'
                 }
               , { href:         contextPath + '/parties'
                 , iconClass:    'icon-pen'
                 , label:        'Parties'
                 , description:  'Definition of parties which can participate in the game and their objectives.'
                 }
               , { href:         contextPath + '/terrains'
                 , iconClass:    'icon-pen'
                 , label:        'Terrains'
                 , description:  'Terrains which are created when the game starts.'
                 }
               , { href:         contextPath + '/games'
                 , iconClass:    'icon-pen'
                 , label:        'Games'
                 , description:  'An overview of games which were playing this encounter.'
                 }
               , { href:         contextPath + '/issues'
                 , iconClass:    'icon-pen'
                 , label:        'Issues'
                 , description:  'Shows potential issues with the encounter and suggestions how to solve those.'
                 }
               ];

            // { href: contextPath + '/stats', iconClass: 'icon-chart-lines', label: 'Statistics', description: 'Recent games and statistics.' },
            // { href: contextPath + '/admin', iconClass: 'icon-cog', label: 'Admin', description: 'Manage releases and collaborators.' },
    }


    export function
    mainView(objId: string): rmx.View {
        return new rmx.View(() => {
            // FIXME: the lookup can fail.
            var encounter = rmx.data.findById<rmx.Storage.Encounter>(objId).get(null);

            return <any> React.createElement(rmx.Components.Encounter,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                }
            );
        });
    }


    export function
    glueView(objectId: string): rmx.View {
        function body(): any {
            var encounter =
                rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
            return React.createElement(rmx.Components.EncounterGlue,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                }
            );
        }

        return new rmx.View(body);
    }


    export function
    encounterPartiesView(objectId: string): rmx.View {
        function body(): any {
            var encounter =
                rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
            return React.createElement(rmx.Components.EncounterParties,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                }
            );
        }

        return new rmx.View(body);
    }


    export function
    encounterPartyView
    ( objectId : string
    , partyId  : string
    ): rmx.View {
        function body(): any {
            var encounter =
                rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
            return React.createElement(rmx.Components.EncounterParty,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                , party        : Avers.lookupItem(encounter.content.parties, partyId)
                }
            );
        }

        return new rmx.View(body);
    }


    export function
    gamesView(objectId: string): rmx.View {
        function body(): any {
            var encounter =
                rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);
            return React.createElement(rmx.Components.EncounterGames,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                }
            );
        }

        return new rmx.View(body);
    }


    export function
    encounterImagesView(objectId: string): rmx.View {
        function body(): any {
            var encounter =
                rmx.data.findById<rmx.Storage.Encounter>(objectId).get(null);

            return React.createElement(rmx.Components.EncounterImagesView,
                { sidebarItems : mkSidebarItems(encounter)
                , encounter    : encounter
                }
            );
        }

        return new rmx.View(body);
    }
}
