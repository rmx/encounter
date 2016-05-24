---
title: Particle Effect
tags: [particles, example]
---

# Particle Effect

We implemented a particle engine based on the paradigm of
[SPARKS.js](https://github.com/zz85/sparks.js).
Particles are emitted from a source emitter with some specific characteristics
(denoted `initializers`), e.g.  color and size, and can be influenced with the
help of `modifiers`. As the name suggests the `initializers` affect the
particle properties at creation where `modifiers` are applied as particles age.

We plan to support different renderers (hardware and software) where only the
software renderer is currently implemented and usable.


## Effect Specification

Particle effects are specified in JSON objects, e.g. the following implements
a simple particle effect:

```language-json
{
    "source": {
        "blending": "custom",
        "counter" : "steady",
        "npart"   : 400,
        "color"   : [100, 1, 1],
        "size"    : [0.2, 0.7],
        "offset"  : [0, 0, 0.2],
        "zone"    : { "DiscZone" : ["rel 0 0 0 target", 0, 0, 1, 0.6, 0.3]}
    },
    "initializers": [
        {"lifetime" : [0.5, 1.4]}
    ],
    "modifiers": [
        {"move"        : []},
        {"age"         : []},
        {"accelerate"  : [0.0, 0.0, 5]},
        {"randomdrift" : [10, 10, 5]},
        {"resize"      : [2.5, 2.6]},
        {"colorshift"  : [80, 1, 0.6]}
    ]
}
```

A particle effect has a `source` entry characterizing the particle emitter and
two lists of `initializers` and `modifiers`. In order to describe meaningful
positions in particle effects the definition distinguishes two position types:

* `abs`: absolute (coordinates or unit), and
* `rel`: relative (unit or labeled vertex).

For example in the effect definition above we have,

    "DiscZone": ["rel 0 0 0 target", 0, 0, 1, 0.6, 0.3]

The exact specification of the position depends on the spell target. At run
time the target is resolved to world objects. Since it is a relative source
the position of the effect is updated if the target moves. Aside from the
`target` specifier, the `self` label is always defined and points to the
currently controlled unit.

Particle effects for spells and auras can be set in the *sensous* editor tab
(along with sounds and animations). The duration of the spell and aura phases
imply different particle source behaviors:

- begin_cast    : shot
- casting       : any
- casting_pulse : shot
- end_cast      : shot
- projectile    : any
- hit_target    : shot

and

- aura_start  : shot
- aura_steady : shot
- aura_tick   : any
- aura_end    : shot


## Ground Area Effects

TBD.


## Zone

Zones are used to describe surfaces or volumes where particles are emitted or
modifiers are applied. The following zones are currently available:

* `CubeZone`
* `DiscZone`
* `LineZone`
* `PointZone`
* `ParallelogramZone`
* `SphereCapZone`

In the following we briefly introduce the arguments of these zones.


### "CubeZone": [position, x, y, z]

Creates a cube zone with origin `position` and `x` width, `y` depth and `z`
height.


### "DiscZone": [center, radiusNormal, outerRadius, innerRadius]

Creates a "2D" disc perpendicular to the specified `radiusNormal` around
`center` with the radius `outerRadius`. If an `innerRadius` is specified the
disc will a hole.


### "LineZone": [start, end]

Creates a line zone form `start` to `end` position.


### "PointZone": [position]

Creates a point zone in `position`.


### "ParallelogramZone": [corner, side1, side2]

Creates a rectangular like shape spanned from the origin in `corner` to both
side endpoints.


### "SphereCapZone": [origin, minr, maxr, phi]

Creates a half dome zone centered in the specified origin between `minr` and
`maxr` with an opening of `phi`



## Source

The source field completely specifies the particle emitter.

* `duration`: travel time to target (in seconds). Only used for projectiles
              and require a `target` field
* `delay`: start the particle effect with a delay (in seconds)
* `counter`: either `steady` (emits particles over time) or `shot` (emits all
             `npart` particles at once)
* `npart`: number of particles to emit
* `color`: color of the particle in HSV
* `size: [min, max]` size of particle
* `offset`: offset from `source`/`target` position if used
* `zone`:


## Initializers

The available initializers are:

* `"lifetime": [min, max]`: lifetime of a particle
* `"velocity": [zone]`: set velocity according to position in zone
* `"radialvelocity": [outerRadius, innerRadius]`: helper to accelerate outward


## Modifiers

Some modifiers use TWEEN.js for interpolation. The default in all cases is
`linear.none`. See
[complete list](https://sole.github.com/tween.js/examples/03_graphs.html) for
available easing schemes.

At the moment we support the following modifiers:

* `"accelerate": [direction x, direction y, direction z]`: accelerate in
  specified 3D direction,
* `"acceleratefactor": [factor of velocity]`: accelerate based on `factor` of
  particles absolute velocity,
* `"acceleratevelocity": [factor based on velocity direction]`: adapt velocity
  by factor of velocity direction,
* `"age": [easing]`: age particle using tween easing (default: `linear.none`),
* `"colorshift": [target hue, target saturation, target value, easing]`:
  interpolate from particle color to target color using easing (default:
  linear),
* `"resize": [target min, target max, easing]`: interpolate from particle
  size to target particle size using easing (default: linear),
* `"move": []`: move particles, and
* `"randomdrift": [drift x, drift y, drift z]`: random drift by max. delta
  drift.

**Note**: If the particle effect has no `move` modifier the particles will not
move and the `age` is necessary for modifiers relying on the particle age.


## Deathzone

Deathzones can be used to remove particles traveling through a specific zone.
For example a rectangular zone can be used to block particles from traveling
through an opening or window.

Currently we only support `CubeZone` as deadzone (more will follow soon):

    "deathzone": { "CubeZone": [-50, -50, 0, 100, 100, -5] }

