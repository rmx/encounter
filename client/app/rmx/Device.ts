module rmx {

    // ParticleEffectsPreferences
    // -----------------------------------------------------------------------

    export class ParticleEffectsPreferences {

        enabled : boolean;
        // ^ Option to disable particle effects globally.

    }


    Avers.definePrimitive(ParticleEffectsPreferences, 'enabled', true);



    // GraphicsPreferences
    // -----------------------------------------------------------------------

    export class GraphicsPreferences {
        particleEffects : ParticleEffectsPreferences;
    }

    Avers.defineObject(GraphicsPreferences, 'particleEffects', ParticleEffectsPreferences, {});



    // AudioPreferences
    // -----------------------------------------------------------------------

    export class AudioPreferences {
        enabled : boolean;
    }

    Avers.definePrimitive(AudioPreferences, 'enabled', true);



    // Preferences
    // -----------------------------------------------------------------------
    //
    // Per-user preferences. The preferences are device local, not synchronized
    // with the server.

    export class Preferences {
        id       : string;

        graphics : GraphicsPreferences;
        audio    : AudioPreferences;
    }

    Avers.defineObject(Preferences, 'graphics', GraphicsPreferences, {});
    Avers.defineObject(Preferences, 'audio', AudioPreferences, {});



    // Device
    // -----------------------------------------------------------------------

    export class Device {

        login : string;
        // ^ The last used login. If present then that is used to pre-fill
        // the input field on the login page.

        preferences : Avers.Collection<Preferences>;
        // ^ Per-user preferences. The 'id' of each item is the 'AccountId'
        // of the user for which the preferences are stored.
    }

    Avers.definePrimitive(Device, 'login', '');
    Avers.defineCollection(Device, 'preferences', Preferences);



    // accountPreferences
    // -----------------------------------------------------------------------
    //
    // Get the account preferences from the device. If a 'Preferences' object
    // doesn't exist yet it will be created and placed into the device.

    export function
    accountPreferences(device: Device, accountId: string): Preferences {
        var prefs = Avers.lookupItem(device.preferences, accountId);
        if (!prefs) {
            prefs = Avers.mk(Preferences, { id: accountId });
            device.preferences.push(prefs);
        }

        return prefs;
    }



    // mkDevice
    // -----------------------------------------------------------------------
    //
    // Create a new 'Device' and keep it synchronized with 'localStorage'.

    export function
    mkDevice(): Device {
        var device = Avers.mk<Device>(Device, {})
          , key    = 'rmx:device'
          , data   = localStorage.getItem(key);


        // If the device data is stored in local storage, try to parse it and
        // load it into the object.
        if (data) {
            try {
                Avers.updateObject(device, JSON.parse(data));
                Avers.deliverChangeRecords(device);
                Avers.migrateObject(device);

            } catch (e) {
                console.log('Failed to parse device data', e);
            }
        }


        // When the device changes, save the data in local storage.
        Avers.attachChangeListener(device, () => {
            localStorage.setItem(key, JSON.stringify(Avers.toJSON(device)));
        });


        return device;
    }
}
