/// <reference path="./Expression.ts" />
/// <reference path="./ExtensibleExpression.ts" />
/// <reference path="./Reference.ts" />
/// <reference path="./Spell.ts" />
/// <reference path="./SensousEffect.ts" />
/// <reference path="./Tick.ts" />

module rmx.Storage {

    export class AuraUniquenessConstraint {

        condition         : string;
        violationResolver : Expression;


        static def =
            { condition         : 'caster-target-category'
            , violationResolver : {}
            };


        useTemplate(name: string): void {
            switch (name) {
            case 'global-replace':
                this.condition = 'target-category';
                this.violationResolver.source = 'newAura';
                break;

            case 'local-replace':
                this.condition = 'caster-target-category';
                this.violationResolver.source = 'newAura';
                break;

            case 'local-stacking':
                this.condition = 'caster-target-category';
                this.violationResolver.source = '{ duration: existingAura.duration, stacks: existingAura.stacks + 1 }';
                break;

            default:
                console.error('Unhandled AUC constraint:', name);
            }
        }
    }

    Avers.definePrimitive(AuraUniquenessConstraint, 'condition');
    Avers.defineObject(AuraUniquenessConstraint, 'violationResolver', Expression, {});



    export var auraModifierNames =
        [ 'maxHealth'
        , 'movementSpeed'
        ];

    export var auraCategoryNames =
        [ 'none'
        , 'potion'
        , 'damage-absorbing-shield'
        , 'total-damage-immunity'
        , 'orb'
        ];



    export class SpellEffectImmunity<T> {
        type    : string;
        content : T;

        static mk(): SpellEffectImmunity<any> {
            return Avers.mk<SpellEffectImmunity<any>>(SpellEffectImmunity,
                { type    : 'expression'
                , content : {}
                }
            );
        }
    }

    var spellEffectImmunityTypes =
        { 'expression' : Expression
        };

    Avers.defineVariant(SpellEffectImmunity, 'content', 'type', spellEffectImmunityTypes, new Expression);



    // AbsorbSpellDamage
    // -----------------------------------------------------------------------

    export class AbsorbSpellDamage {

        predicate : ExtensibleExpression<any>;
        // ^ Selects to which damage this applies to.

        amount : ExtensibleExpression<any>;
        // ^ The amount to absorb.

        action : ExtensibleExpression<any>;
        // ^ What to do when some damage is absorbed.
    }


    Avers.defineObject(AbsorbSpellDamage, 'predicate', ExtensibleExpression, { type: 'expression', content: {} });
    Avers.defineObject(AbsorbSpellDamage, 'amount',    ExtensibleExpression, { type: 'expression', content: {} });
    Avers.defineObject(AbsorbSpellDamage, 'action',    ExtensibleExpression, { type: 'expression', content: {} });



    export class AttributeModifier<T> {
        name    : string;
        type    : string;
        content : T;

        static mk(): AttributeModifier<Expression> {
            return Avers.mk<AttributeModifier<Expression>>(AttributeModifier,
                { name    : 'maxHealth'
                , type    : 'expression'
                , content : {}
                }
            );
        }
    }

    var attributeModifierTypes =
        { 'expression' : Expression
        };

    Avers.definePrimitive(AttributeModifier, 'name', '');
    Avers.defineVariant  (AttributeModifier, 'content', 'type', attributeModifierTypes, new Expression);



    export class Possess {
        static mk(): Possess {
            return Avers.mk<Possess>(Possess, {});
        }
    }

    Avers.declareConstant(Possess);


    export class SubstituteBehavior {
        behavior : Reference;

        static mk(): SubstituteBehavior {
            return Avers.mk<SubstituteBehavior>(SubstituteBehavior, {});
        }
    }

    Avers.defineObject(SubstituteBehavior, 'behavior', Reference, {});



    export class Taunt {
        static mk(): Taunt {
            return Avers.mk<Taunt>(Taunt, {});
        }
    }

    Avers.declareConstant(Taunt);



    export class Stun {
        static mk(): Stun {
            return Avers.mk<Stun>(Stun, {});
        }
    }

    Avers.declareConstant(Stun);



    export class Petrify {
        static mk(): Petrify {
            return Avers.mk<Petrify>(Petrify, {});
        }
    }

    Avers.declareConstant(Petrify);



    export class AuraEffect<T> {
        id     : string;
        effect : T;
    }

    export var auraEffectTypes =
        { 'absorb-spell-damage'   : AbsorbSpellDamage
        , 'attribute-modifier'    : AttributeModifier
        , 'possess'               : Possess
        , 'spell-effect-immunity' : SpellEffectImmunity
        , 'stun'                  : Stun
        , 'petrify'               : Petrify
        , 'substitute-behavior'   : SubstituteBehavior
        , 'taunt'                 : Taunt
        };

    Avers.defineVariant(AuraEffect, 'effect', 'type', auraEffectTypes);



    // -----------------------------------------------------------------------
    export class AuraDuration {
        enabled : boolean;
        content : Radius;
    }

    Avers.definePrimitive(AuraDuration, 'enabled', false);
    Avers.defineObject   (AuraDuration, 'content', Radius, {});



    // -----------------------------------------------------------------------
    export class EventHandler<T> {
        event   : string;
        content : T;
    }

    export var eventHandlerTypes =
        { 'expression' : Expression
        };

    Avers.definePrimitive(EventHandler, 'event');
    Avers.defineVariant  (EventHandler, 'content', 'type', eventHandlerTypes);

    // -----------------------------------------------------------------------
    export class AuraSensousEffects {
        auraStart  : SensousEffectState;
        auraSteady : SensousEffectState;
        auraTick   : SensousEffectState;
        auraEnd    : SensousEffectState;
    }

    Avers.defineObject(AuraSensousEffects, 'auraStart', SensousEffectState, {});
    Avers.defineObject(AuraSensousEffects, 'auraSteady', SensousEffectState, {});
    Avers.defineObject(AuraSensousEffects, 'auraTick', SensousEffectState, {});
    Avers.defineObject(AuraSensousEffects, 'auraEnd', SensousEffectState, {});



    // -----------------------------------------------------------------------
    export class Aura {

        name                 : string;
        description          : string;
        category             : string;
        uniquenessConstraint : AuraUniquenessConstraint;
        icon                 : Reference;
        iconId               : string;
        effects              : AuraEffect<any>[];
        duration             : AuraDuration;
        tickTimer            : TickTimer<any>;
        eventHandlers        : EventHandler<any>[];
        sensousEffects       : AuraSensousEffects;

        energy : number;
        // ^ The amount of energy the aura starts with. Can be zero to
        // indicate that the aura doesn't use any energy.

        stackCount : number;
        // ^ The number of stacks the aura starts with. Can be zero if the
        // aura doesn't use stacks.

        static mk(name: string) {
            return Avers.mk<Aura>(Aura,
                { type : 'aura'
                , name : name
                }
            );
        }
    }

    Avers.definePrimitive  (Aura, 'name');
    Avers.definePrimitive  (Aura, 'description');
    Avers.definePrimitive  (Aura, 'category', 'none');
    Avers.definePrimitive  (Aura, 'iconId');
    Avers.defineObject     (Aura, 'uniquenessConstraint', AuraUniquenessConstraint, AuraUniquenessConstraint.def);
    Avers.defineObject     (Aura, 'icon', Reference, {});
    Avers.defineCollection (Aura, 'effects', AuraEffect);
    Avers.defineObject     (Aura, 'duration', AuraDuration, {});
    Avers.defineObject     (Aura, 'tickTimer', TickTimer, { type: 'interval', content: {} });
    Avers.defineCollection (Aura, 'eventHandlers', EventHandler);
    Avers.definePrimitive  (Aura, 'energy', 0);
    Avers.definePrimitive  (Aura, 'stackCount', 0);
    Avers.defineObject     (Aura, 'sensousEffects', AuraSensousEffects, {});
}
