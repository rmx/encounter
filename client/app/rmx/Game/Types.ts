/// <reference path="../Pure/Ids.ts" />
/// <reference path="../Pure/Game.ts" />
/// <reference path="../Pure/Game/Events.ts" />

module rmx.Game {

    export class AuraTickMessage {
        constructor
            ( public aura : rmx.Pure.Aura
            ) {}
    }

    export class SpellcastFinish {
        constructor
          ( public casterId    : rmx.Pure.EntityId
          , public spell       : rmx.Pure.Spell
          , public targetInfos : rmx.Pure.TargetInfo[]
          ) {}
    }
}
