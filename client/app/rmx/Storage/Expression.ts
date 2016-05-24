module rmx.Storage {

    export class Expression {
        language : string;
        source   : string;
    }

    Avers.definePrimitive(Expression, 'language', '');
    Avers.definePrimitive(Expression, 'source', '');
}
