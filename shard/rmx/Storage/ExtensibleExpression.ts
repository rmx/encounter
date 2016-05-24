
import * as Avers from '../../vendor/avers';
import { Expression } from './Expression';



// ExtensibleExpression
// -----------------------------------------------------------------------
//
// A type which is essentially a sumtype of Expression or some other type.
// This construction is used in many places, in particular close to
// leaf positions.

export class ExtensibleExpression<T> {
    content : T;

    static mk(): ExtensibleExpression<Expression> {
        return Avers.mk<ExtensibleExpression<Expression>>(ExtensibleExpression,
            { type    : 'expression'
            , content : {}
            }
        );
    }
}

Avers.defineVariant(ExtensibleExpression, 'content', 'type', { expression: Expression }, new Expression);
