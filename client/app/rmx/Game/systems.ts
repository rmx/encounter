/// <reference path="./State.ts" />

module rmx.Game {

    export interface ISystem {

        update(state: rmx.Game.State, now: number, dt: number): void;

    }

}
