
import { Aura, Spell } from '../../Pure/Game';
import { defineAccessor } from '../../Game/Env';
import { WorldObject } from '../../Game/Entities/WorldObject';



export function mk
( entity      : WorldObject
, spell       : Spell
, aura        : Aura
, spellEffect : any
) {
    var env: any = {};

    env.module = {};

    defineAccessor(env, 'entity', function() {
        return entity;
    });

    defineAccessor(env, 'spell', function() {
        return spell;
    });

    defineAccessor(env, 'aura', function() {
        return aura;
    });

    defineAccessor(env, 'spellEffect', function() {
        return spellEffect;
    });


    return env;
}
