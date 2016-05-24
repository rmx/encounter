
import * as Avers from '../../vendor/avers';

import { TickTimer } from './Tick';
import { Reference } from './Reference';
import { Expression } from './Expression';
import { SensousEffectState } from './SensousEffect';
import { Range } from './Range';



// -----------------------------------------------------------------------
export class Script {

    source : string;

    static mk(source: string): Script {
        return Avers.mk<Script>(Script, { source: source });
    }
}

Avers.definePrimitive(Script, 'source');



// -----------------------------------------------------------------------
export class Constant<T> {

    value : T;

    static mk<T>(value: T): Constant<T> {
        return Avers.mk<Constant<T>>(Constant, { value: value });
    }
}

Avers.definePrimitive(Constant, 'value');



// -----------------------------------------------------------------------
export class CastTime {
    min  : number;
    base : number;
    max  : number;
}

Avers.definePrimitive(CastTime, 'min',  0);
Avers.definePrimitive(CastTime, 'base', 1.5);
Avers.definePrimitive(CastTime, 'max',  2);



// -----------------------------------------------------------------------
export class Cooldown {
    min  : number;
    base : number;
    max  : number;
}

Avers.definePrimitive(Cooldown, 'min',  0);
Avers.definePrimitive(Cooldown, 'base', 1.5);
Avers.definePrimitive(Cooldown, 'max',  2);



// -----------------------------------------------------------------------
export class SpellCooldown {
    enabled : boolean;
    content : Cooldown;
}

Avers.definePrimitive(SpellCooldown, 'enabled', false);
Avers.defineObject(SpellCooldown, 'content', Cooldown, {});



// -----------------------------------------------------------------------
export class SpellPulseTimer {
    enabled : boolean;
    content : TickTimer<any>;
}

Avers.definePrimitive(SpellPulseTimer, 'enabled', false);
Avers.defineObject(SpellPulseTimer, 'content', TickTimer, { type:
    'interval', content: {} } );



// -----------------------------------------------------------------------
export class Radius {
    min  : number;
    base : number;
    max  : number;
}

Avers.definePrimitive(Radius, 'min',  3);
Avers.definePrimitive(Radius, 'base', 5);
Avers.definePrimitive(Radius, 'max',  7);



// -----------------------------------------------------------------------
export class PowerCost {
    powerType : string;
    amount    : number;
}

Avers.definePrimitive(PowerCost, 'powerType', 'energy');
Avers.definePrimitive(PowerCost, 'amount', 3);



// -----------------------------------------------------------------------
export class Dummy {
    static mk(): Dummy {
        return Avers.mk<Dummy>(Dummy, {});
    }
}

Avers.declareConstant(Dummy);


// -----------------------------------------------------------------------
export class Heal {

    amount : number;

    static mk(): Heal {
        return Avers.mk<Heal>(Heal, { amount: 100 });
    }
}

Avers.definePrimitive(Heal, 'amount', 50);



// -----------------------------------------------------------------------
export class SpellDamage {

    school : string;
    amount : number;

    static mk(): SpellDamage {
        return Avers.mk<SpellDamage>(SpellDamage, { amount: 100 });
    }
}

Avers.definePrimitive(SpellDamage, 'school', 'shadow');
Avers.definePrimitive(SpellDamage, 'amount', 50);



// -----------------------------------------------------------------------
export class Leap {}
Avers.declareConstant(Leap);



// -----------------------------------------------------------------------
export class Charge {}
Avers.declareConstant(Charge);



// -----------------------------------------------------------------------
export class InterruptSpellcast {}
Avers.declareConstant(InterruptSpellcast);



// -----------------------------------------------------------------------
export class ApplyAura {
    aura : Reference;
}

Avers.defineObject   (ApplyAura, 'aura', Reference, {});



// -----------------------------------------------------------------------
export class ModifyAurasPredicate<T> {
    content : T;
}

export var modifyAurasPredicateTypes =
    { expression : Expression
    };

Avers.defineVariant(ModifyAurasPredicate, 'content', 'type', modifyAurasPredicateTypes);



// -----------------------------------------------------------------------
export class ModifyAurasAction<T> {
    content : T;
}

export var modifyAurasActionTypes =
    { expression : Expression
    };

Avers.defineVariant(ModifyAurasAction, 'content', 'type', modifyAurasActionTypes);



// -----------------------------------------------------------------------
export class ModifyAuras {

    predicate : ModifyAurasPredicate<any>;
    // ^ The predicate selects to which auras the modifications should
    // apply.

    action : ModifyAurasAction<any>;
    // ^ The action to execute each time.

    maxApplications : number;
    // ^ How many times the modifications should apply in total
    // on a single entity.

    perAuraLimit : number;
    // ^ The number of times the action should apply to a particular aura.

    maxAffectedAuras : number;
    // ^ To how many auras the modification should apply.
}

Avers.defineObject   (ModifyAuras, 'predicate', ModifyAurasPredicate, { type: 'expression', content: {} });
Avers.defineObject   (ModifyAuras, 'action', ModifyAurasAction, { type: 'expression', content: {} });
Avers.definePrimitive(ModifyAuras, 'maxApplications', 1);
Avers.definePrimitive(ModifyAuras, 'perAuraLimit', 999);
Avers.definePrimitive(ModifyAuras, 'maxAffectedAuras', 999);



// -----------------------------------------------------------------------
export class GroundArea {
    aura     : Reference;
    duration : CastTime; // FIXME: A better type for min/base/max
}

Avers.defineObject(GroundArea, 'aura',     Reference, {});
Avers.defineObject(GroundArea, 'duration', CastTime,  {});



// TerrainPosition
// -----------------------------------------------------------------------
//
// A TerrainPosition in the Storage module is a tuple of Terrain EntityId
// and the name of a PointOfInterest. These two are resolved at runtime to
// an actual position in the terrain. If multiple points of interest with
// that name exist, a random one is taken.

export class TerrainPosition {
    terrainEntityId     : string;
    pointOfInterestName : string;
}

Avers.definePrimitive(TerrainPosition, 'terrainEntityId', '');
Avers.definePrimitive(TerrainPosition, 'pointOfInterestName', '');


// Similar to ExpensibleExpression but currently only supports
// TerrainPosition content.
export class TeleportDestination {
    content : TerrainPosition;
}

export var teleportDestinationTypes =
    { 'terrain-position' : TerrainPosition
};

Avers.defineVariant(TeleportDestination, 'content', 'type', teleportDestinationTypes, {});


// -----------------------------------------------------------------------
export class Teleport {

    destination : TeleportDestination;

    static mk(): Teleport {
        return Avers.mk<Teleport>(Teleport, { destination: { } });
    }
}

Avers.defineObject(Teleport, 'destination', TeleportDestination, { type: 'terrain-position', content: {} });



// -----------------------------------------------------------------------
export class TRFSelf {

    get inputType() {
        return 'ignored';
    }
    get outputType() {
        return 'world-object';
    }
}

Avers.declareConstant(TRFSelf);



// -----------------------------------------------------------------------
export class TRFParty {

    get inputType() {
        return 'ignored';
    }
    get outputType() {
        return '[world-object]';
    }
}

Avers.declareConstant(TRFParty);



// -----------------------------------------------------------------------
export class TRFPartyInArea {

    get inputType() {
        return 'location';
    }
    get outputType() {
        return '[world-object]';
    }
}

Avers.declareConstant(TRFPartyInArea);



// -----------------------------------------------------------------------
export class TRFEnemiesInArea {

    get inputType() {
        return 'location';
    }
    get outputType() {
        return '[world-object]';
    }
}

Avers.declareConstant(TRFEnemiesInArea);



// -----------------------------------------------------------------------
export class TargetResolverFunction {
    content : any;
}

export var targetResolverFunctionTypes =
    { 'self'            : TRFSelf
    , 'party'           : TRFParty
    , 'party-in-area'   : TRFPartyInArea
    , 'enemies-in-area' : TRFEnemiesInArea
    };

Avers.defineVariant(TargetResolverFunction, 'content', 'type', targetResolverFunctionTypes);



// -----------------------------------------------------------------------
export class TargetResolver {
    chain : TargetResolverFunction[];
}

Avers.defineCollection(TargetResolver, 'chain', TargetResolverFunction);



// -------------------------------------------------------------------
export class SpellEffect<T> {

    id             : string;
    targetResolver : TargetResolver;
    effect         : T;

    static mk<T>(): SpellEffect<T> {
        var effect = Avers.toJSON(Dummy.mk());
        return Avers.mk<SpellEffect<T>>(SpellEffect, { effect: effect });
    }
}

export var spellEffectTypes =
    { 'dummy'               : Dummy
    , 'apply-aura'          : ApplyAura
    , 'charge'              : Charge
    , 'ground-area'         : GroundArea
    , 'heal'                : Heal
    , 'interrupt-spellcast' : InterruptSpellcast
    , 'leap'                : Leap
    , 'modify-auras'        : ModifyAuras
    , 'spell-damage'        : SpellDamage
    , 'teleport'            : Teleport
    };

Avers.defineObject(SpellEffect, 'targetResolver', TargetResolver, {});
Avers.defineVariant(SpellEffect, 'effect', 'type', spellEffectTypes);



// -----------------------------------------------------------------------
export class SpellSensousEffects {
    castBegin    : SensousEffectState;
    casting      : SensousEffectState;
    castingPulse : SensousEffectState;
    castEnd      : SensousEffectState;
    projectile   : SensousEffectState;
    spellHit     : SensousEffectState;
}

Avers.defineObject(SpellSensousEffects, 'castBegin', SensousEffectState, {});
Avers.defineObject(SpellSensousEffects, 'casting', SensousEffectState, {});
Avers.defineObject(SpellSensousEffects, 'castingPulse', SensousEffectState, {});
Avers.defineObject(SpellSensousEffects, 'castEnd', SensousEffectState, {});
Avers.defineObject(SpellSensousEffects, 'projectile', SensousEffectState, {});
Avers.defineObject(SpellSensousEffects, 'spellHit', SensousEffectState, {});



// -----------------------------------------------------------------------
export class Projectile {

    enabled : boolean;
    // ^ If true, the spell creates projectile which fly towards the
    // resolved spell targets. This delays spell effect application.

    speed : number;
    // ^ The speed at which the projectile flies towards the target. The
    // delay is calculated when the projectile is created and remains
    // constant. The projectile is guaranteed to reach the target after
    // the calculated delay.
}

Avers.definePrimitive(Projectile, 'enabled', false);
Avers.definePrimitive(Projectile, 'speed',   10);



// -----------------------------------------------------------------------
export class Spell {

    name        : string;
    description : string;
    iconId      : string;

    targetType  : string;

    range       : Range;
    castTime    : CastTime;
    radius      : Radius;
    powerCost   : PowerCost;
    cooldown    : SpellCooldown;
    pulseTimer  : SpellPulseTimer;

    effects     : SpellEffect<any>[];

    sensousEffects : SpellSensousEffects;

    projectile  : Projectile;


    static mk(name: string): Spell {
        return Avers.mk<Spell>(Spell,
            { name: name
            }
        );
    }
}

Avers.definePrimitive (Spell,  'name');
Avers.definePrimitive (Spell,  'iconId');
Avers.definePrimitive (Spell,  'description');
Avers.definePrimitive (Spell,  'targetType', 'world-object');

Avers.defineObject    (Spell,  'range',       Range, { min: 0, max: 40 });
Avers.defineObject    (Spell,  'castTime',    CastTime, {});
Avers.defineObject    (Spell,  'radius',      Radius, {});
Avers.defineObject    (Spell,  'powerCost',   PowerCost, {});
Avers.defineObject    (Spell,  'cooldown',    SpellCooldown, {});
Avers.defineObject    (Spell,  'sensousEffects', SpellSensousEffects, {});
Avers.defineObject    (Spell,  'projectile',  Projectile, {});
Avers.defineObject    (Spell,  'pulseTimer',  SpellPulseTimer, {});

Avers.defineCollection(Spell,  'effects',   SpellEffect);
