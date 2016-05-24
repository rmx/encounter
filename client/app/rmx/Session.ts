/// <reference path="./Pure/Ids.ts" />

module rmx {

    // TODO: Use promises instead of explicit then callbacks (this applies to
    // the signin / signup functions defined further down in this file).


    // TODO: Move error into rmx.data. This indicates an unrecoverable error
    // which the client does not know how to deal with.
    export var error = null;


    // TODO: Clean this up and draw a proper state diagram. I suspect most
    // of these states are not needed.
    export enum SessionStatus
        { Unknown
        , Restoring
        , Authenticated
        , Anonymous
        , Authenticating
        , AuthenticationFailed
        , Error
        , Unauthenticated
        }



    // -----------------------------------------------------------------------
    export class Session {
        status    : SessionStatus;
        accountId : rmx.Pure.AccountId;
    }

    Avers.definePrimitive(Session, 'status',    SessionStatus.Unknown);
    Avers.definePrimitive(Session, 'accountId', null);



    // restoreSession
    // -----------------------------------------------------------------------
    //
    // Attempt to determine the session status by contacting the server. The
    // server returns either 404 if no session exists, or 200 and includes
    // the AccountId in the response.

    export function
    restoreSession(session: Session): void {
        session.status = rmx.SessionStatus.Restoring;

        request
            .get(rmx.apiUrl('/session'))
            .withCredentials()
            .end(function(res: any) {
                if (res.status === 200) {
                    session.status    = rmx.SessionStatus.Authenticated;
                    session.accountId = res.body.accountId;

                } else {
                    session.status    = rmx.SessionStatus.Anonymous;
                }
            }).on('error', function(e) {
                error = e.message;
                session.status = rmx.SessionStatus.Error;
            });
    }


    export function
    signup(session: Session, login: string, then?: Function): void {
        session.status = rmx.SessionStatus.Authenticating;

        request
            .post(rmx.apiUrl('/account'))
            .withCredentials()
            .send({ login: login })
            .end(function(res: any) {
                if (res.status === 200) {
                    signin(session, res.body.accountId, '', then);

                } else {
                    session.status = rmx.SessionStatus.AuthenticationFailed;
                    then();
                }
            });
    }


    export function
    signin(session: Session, username: string, password: string, then?: Function): void {
        session.status = rmx.SessionStatus.Authenticating;

        request
            .post(rmx.apiUrl('/session'))
            .withCredentials()
            .send({ login: username, secret: password })
            .end(function(res: any) {
                if (res.status === 200) {
                    session.status    = rmx.SessionStatus.Authenticated;
                    session.accountId = res.body.accountId;

                } else {
                    session.status = rmx.SessionStatus.AuthenticationFailed;
                }

                if (then) { then(); }
            });
    }


    export function
    signout(session: Session): void {
        session.status = rmx.SessionStatus.Unauthenticated;

        request
            .del(rmx.apiUrl('/session'))
            .withCredentials()
            .end(function() {
                session.status    = rmx.SessionStatus.Anonymous;
                session.accountId = null;
            });
    }
}
