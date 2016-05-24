
import { Aura, Projectile } from '../../Pure/Game';
import { WorldObject } from '../../Game/Entities/WorldObject';
import { Spell } from '../../Pure/Game';
import * as Event from '../../Pure/Game/Events';
import { Kill, Damage, Heal, Environment } from '../../Pure/Game/Events';



export function mk() {
    var env: any = {};


    env.Spell                = Spell;
    env.Aura                 = Aura;
    env.Environment          = Environment;
    env.Projectile           = Projectile;


    env.PowerRegenEvent      = Event.PowerRegenEvent;
    env.CombatEvent          = Event.CombatEvent;
    env.EntityMutationEvent  = Event.EntityMutationEvent;
    env.RelocationEvent      = Event.RelocationEvent;


    env.Heal                 = Heal;
    env.Damage               = Damage;
    env.Kill                 = Kill;



    // TODO: Not sure whether to expose the Pure version or the Entity
    // version. Or perhaps try to merge the two.
    //
    // env.GroundAreaEffect = GroundAreaEffect / rmx.Game.GroundAreaEffect;


    env.WorldObject          = WorldObject;


    return env;
}
