/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../CollectionItemHeader.ts" />
/// <reference path="../NumberInput.ts" />
/// <reference path="../ReferencePicker.ts" />

/// <reference path="./TargetResolver.ts" />
/// <reference path="./Expression.ts" />

/// <reference path="../../Core/Encounter.ts" />


module rmx.Component.Editor {

    export interface SpellEffectProps {
        encounter : rmx.data.Object<rmx.Storage.Encounter>;
        spell     : rmx.Storage.Spell;
        effect    : rmx.Storage.SpellEffect<any>;
    }

    class SpellEffectSpec extends ReactComponent<SpellEffectProps, {}> {

        render() {
            var encounter = this.props.encounter
              , effect    = this.props.effect;

            var targetResolver = TargetResolver({ targetResolver: effect.targetResolver });
            var type = Avers.typeName(rmx.Storage.spellEffectTypes, effect.effect.constructor);

            return React.DOM.div
                ( {}
                , CollectionItemHeader({ id: effect.id, text: type, onRemove: this.removeEffect })
                , targetResolver
                , effectE(encounter, type, effect.effect)
                );
        }

        removeEffect() {
            rmx.data.removeItem(this.props.spell.effects, this.props.effect);
        }
    }

    export var SpellEffect = createClass(SpellEffectSpec);


    function effectE(encounter, type, effect) {
        // XXX: tslint: duplicate variable
        var searchResult = null;

        switch (type) {
        case 'dummy':
            return React.DOM.div({}, 'dummy');


        case 'heal':
            return React.DOM.div
                ( { className: 'form' }
                , React.DOM.label({}, 'amount')
                , NumberInput({ object: effect, field: 'amount' })
                );


        case 'spell-damage':
            function changeSpellSchool(ev) {
                effect.school = ev.target.value;
            }

            return React.DOM.div
                ( { className: 'form' }

                , React.DOM.label({}, 'school')
                , React.DOM.select
                    ( { value: effect.school, onChange: changeSpellSchool }
                    , React.DOM.option({ value: 'acid' }, 'Acid')
                    , React.DOM.option({ value: 'arcane' }, 'Arcane')
                    , React.DOM.option({ value: 'electric' }, 'Electric')
                    , React.DOM.option({ value: 'fire' }, 'Fire')
                    , React.DOM.option({ value: 'frost' }, 'Frost')
                    , React.DOM.option({ value: 'holy' }, 'Holy')
                    , React.DOM.option({ value: 'physical' }, 'Physical')
                    , React.DOM.option({ value: 'poison' }, 'Poison')
                    , React.DOM.option({ value: 'shadow' }, 'Shadow')
                    )

                , React.DOM.label({}, 'amount')
                , NumberInput({ object: effect, field: 'amount' })
                );


        case 'apply-aura':

            searchResult = rmx.Core.auraSearchResult(encounter);

            return React.DOM.div
                ( { className: 'form' }
                , React.DOM.label({}, 'aura')
                , ReferencePicker({ reference: effect.aura, searchResult: searchResult, toPath: rmx.paths.toResourcePath })
                );


        case 'modify-auras':

            return React.DOM.div
                ( { className: 'vertical form' }

                , React.DOM.label({}, 'predicate')
                , Expression({ expr: effect.predicate.content, env: 'Global' })

                , React.DOM.label({}, 'action')
                , Expression({ expr: effect.action.content, env: 'Global' })

                , React.DOM.label({}, 'Max applications')
                , NumberInput({ object: effect, field: 'maxApplications' })

                , React.DOM.label({}, 'Per-aura limit')
                , NumberInput({ object: effect, field: 'perAuraLimit' })

                , React.DOM.label({}, 'Max affected auras')
                , NumberInput({ object: effect, field: 'maxAffectedAuras' })
                );


        case 'leap':
        case 'charge':
        case 'interrupt-spellcast':
            return null;


        case 'ground-area':

            searchResult = rmx.Core.auraSearchResult(encounter);

            return React.DOM.div
                    ( { className: 'vertical form' }

                    , React.DOM.label({}, 'Aura')
                    , ReferencePicker({ reference: effect.aura, searchResult: searchResult, toPath: rmx.paths.toResourcePath })

                    , React.DOM.label({}, 'duration')
                    , React.DOM.div
                        ( { className: 'form-row'}
                        , React.DOM.div
                            ( { className: 'form-row-element' }
                            , React.DOM.label({}, 'min')
                            , NumberInput({ object: effect.duration, field: 'min' })
                            )
                        , React.DOM.div
                            ( { className: 'form-row-element' }
                            , React.DOM.label({}, 'base')
                            , NumberInput({ object: effect.duration, field: 'base' })
                            )
                        , React.DOM.div
                            ( { className: 'form-row-element' }
                            , React.DOM.label({}, 'max')
                            , NumberInput({ object: effect.duration, field: 'max' })
                            )
                        )
                    );


        case 'teleport':
            return React.DOM.div
                ( { className: 'vertical form' }
                , React.DOM.label({}, 'destination')
                , SpawnPoint({ encounter: encounter, spawnPoint: effect.destination.content })
                );


        default:
            return React.DOM.div({}, 'Unknown effect: ', type);
        }
    }
}
