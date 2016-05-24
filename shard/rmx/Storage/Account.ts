
import * as Avers from '../../vendor/avers';
import { Preferences } from './Preferences';



export class Binding {

    id       : string;

    action   : string;
    triggers : string[];


    get primaryTrigger(): string {
        return (this.triggers || [])[0] || '';
    }

    get secondaryTrigger(): string {
        return (this.triggers || [])[1] || '';
    }
}

Avers.definePrimitive(Binding, 'action');
Avers.defineCollection(Binding, 'triggers', String);



export class Bindings {

    bindings : Binding[];

    findBinding(action: string): Binding {
        return this.bindings.filter(function(x) {
            return x.action === action;
        })[0];
    }
}

Avers.defineCollection(Bindings, 'bindings', Binding);



export class Account {

    login       : string;
    email       : string;
    friends     : string[];

    bindings    : Bindings;
    preferences : Preferences;


    static mk(login: string): Account {
        return Avers.mk<Account>(Account, {
            login : login
        });
    }

    get name(): string {
        return this.login;
    }
}

Avers.definePrimitive (Account, 'login');
Avers.definePrimitive (Account, 'email');
Avers.defineCollection(Account, 'friends', String);
Avers.defineObject    (Account, 'bindings', Bindings);
Avers.defineObject    (Account, 'preferences', Preferences);
