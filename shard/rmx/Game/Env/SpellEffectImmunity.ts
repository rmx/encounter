
import { WorldObject } from '../../Game/Entities/WorldObject';
import { Spell } from '../../Pure/Game';
import { defineAccessor } from '../../Game/Env';


export function mk
( entity      : WorldObject
, spell       : Spell
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

    defineAccessor(env, 'spellEffect', function() {
        return spellEffect;
    });


    return env;
}
