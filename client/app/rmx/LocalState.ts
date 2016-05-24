module rmx {

    // -----------------------------------------------------------------------
    export class DrawerState {
        open : boolean;
    }

    Avers.definePrimitive(DrawerState, 'open', false);



    // -----------------------------------------------------------------------
    export class LocalState {
        drawer           : DrawerState;
        collectionFilter : string;
        resourceFilter   : string;
        pickerFilter     : string;
    }

    Avers.defineObject   (LocalState, 'drawer', DrawerState, {});
    Avers.definePrimitive(LocalState, 'collectionFilter', null);
    Avers.definePrimitive(LocalState, 'resourceFilter', null);
    Avers.definePrimitive(LocalState, 'pickerFilter', null);
}
