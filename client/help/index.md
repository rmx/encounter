---
title: Introduction
---

# Introduction

An encounter is a short game where one or more players compete against each
other or a scripted enemy. Encounters are short, even though there is no hard
limit, they should be no longer than 15 minutes.

There is no fixed goal in an encounter. It's up to you to specify when and
which players have won or lost. You can either leave it completely up to the
players to figure that out, or use [Objectives](/concepts/objectives/) to give
players hints what they should do. Here are some ideas:

- Kill the boss.
- Collect as many resources as you can (with a fixed time limit).
- Get through a maze as fast as possible.

Players are grouped into teams. The teams can work together or compete against
each other. When the game ends, the team with the highest score wins.

Players can also collect [Achievements](/concepts/achievements/). They are
awarded for outstanding performance and should not be too easy to earn. The
harder they are to earn, the more they are worth.


## Roles

Before entering the game, players must chose a role. The role acts as
a template and defines which attributes and spells their WorldObject will
have.

This preparation is done in the Wardrobe. Players can inspect the roles, chat,
strategize and otherwise prepare for the game. When all players are ready,
they enter the game.


## WorldObjects

Each [Player](/api/ref/Player/) controls
a [WorldObject](/api/ref/WorldObject/). He can move it around and cast spells
that are available to it. Only one player can control an object at any given
time, but it's possible to change the object that a player controls (as long
as the new object is not already being controlled by somebody).


## Terrains

Each WorldObject is located in a [Terrain](/api/ref/Terrain/), a continuous
geometry in which the object can move. It's made of individual tiles placed on
a grid. An encounter can use multiple terrains and teleport objects (players)
between them.


## Spells

Each WorldObject has access to a number of spells which it can cast. The
[spell script](/scripts/spell/) defines what happens when the object starts to
cast, when it finishes, when it's interrupted etc.

Casting a spell may fail if certain preconditions are not satistfied. Spells
also may have a cast time, that is the time required to perform the spell.
During that time the caster may be interrupted.


## Auras

Auras are attached to WorldObjects and modify or influence how the object
responds to changes in the game. A typical use-case for an aura would be
immunity against a certain kind of damage, or periodicallly healing the
object.


## Cooldowns

Cooldowns limit the number of spells a player can cast. This is to prevent the
server from overloading and to force players to carefully think which before
using a spell (instead of blindly bashing the keyboard).

There are three types of cooldowns: _Global_, _Spell_, _Named_. Each
WorldObject has one global cooldown timer, one timer for each spell cooldown
and one for each named cooldown. Spells can decide which cooldown timers they
want to honor. While any of those cooldowns is active, the spell may not be
used.
