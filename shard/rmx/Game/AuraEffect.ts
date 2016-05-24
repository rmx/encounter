
import { Aura } from '../Pure/Game';
import { State } from '../Game/State';
import { broadcastMessage } from '../Game';
import { attachBehavior } from '../Game/Behavior';
import * as Storage from '../Storage';
import { WorldObject, formulaModifiersChanged,
    auraEffectsOfType } from '../Game/Entities/WorldObject';
import { removeBehavior } from '../Game/Behavior';
import { SMSG_FIELDS } from '../Game/Messages';



export function
attributeModifier
( state      : State
, entity     : WorldObject
, aura       : Aura
, auraEffect : Storage.AuraEffect<Storage.AttributeModifier<any>>
): void {
    // FIXME: unused variable warning.
    (() => { return aura; })();

    var modifierNames = [ auraEffect.effect.name ];
    formulaModifiersChanged(state, entity, modifierNames);
}


export function
stun
( state      : State
, entity     : WorldObject
, aura       : Aura
, auraEffect : Storage.AuraEffect<Storage.Stun>
): void {
    // FIXME: unused variable warning.
    (() => { return aura; })();
    (() => { return auraEffect; })();

    var fields = { stunned: auraEffectsOfType(state, entity, 'stun').length > 0 };
    broadcastMessage(state, SMSG_FIELDS(entity, fields));
}


export function
substituteBehavior
( state      : State
, entity     : WorldObject
, aura       : Aura
, auraEffect : Storage.AuraEffect<Storage.SubstituteBehavior>
): void {
    // FIXME: unused variable warning.
    (() => { return aura; })();

    // Figure out which SubstituteBehavior should be active.
    var effect = auraEffectsOfType<Storage.SubstituteBehavior>(state, entity, 'substitute-behavior')[0];

    // Remove all behaviors which were createt by SubstituteBehavior but
    // which should not be active.
    entity.behaviorStack.filter(x => {
        return x.data &&
               x.data instanceof Storage.SubstituteBehavior &&
               x.data !== effect;
    }).forEach(x => {
        console.log('Removing behavior ' + x.resourceId + ' from ' + entity);
        removeBehavior(state, entity, x.behaviorId);
    });

    // If the effect should be active, attach its behavior.
    if (effect) {
        var resourceId = effect.behavior.toString();

        console.log('Attaching behavior ' + resourceId + ' to ' + entity);
        attachBehavior(state, entity, resourceId, auraEffect.effect);
    }
}
