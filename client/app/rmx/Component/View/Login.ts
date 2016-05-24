/// <reference path="../../data.ts" />
/// <reference path="../../Game.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../Link.ts" />

module rmx.Component.View {

    export interface LoginState {
        email   : string;
        secret  : string;
        message : string;
    }

    class LoginSpec extends ReactComponent<{}, LoginState> {
        getInitialState() {
            var login = rmx.data.device.login;

            return { email   : login
                   , secret  : ''
                   , message : null
                   };
        }

        render() {
            var error = this.state.message
                ? React.DOM.p({ className: 'error' }, this.state.message)
                : null;

            return Body
                ( {}
                , Site
                    ( {}
                    , AppNavBar()
                    , React.DOM.div
                        ( { className: 'rmx login' }
                        , React.DOM.form
                            ( { className: 'login-box form', onSubmit: this.login }

                            , React.DOM.label({}, 'Username or email')
                            , React.DOM.input({ className: 'large', type: 'text', name: 'email', value: this.state.email, onChange: this.changeEmail })

                            , React.DOM.label({}, 'Password')
                            , React.DOM.input({ className: 'large', type: 'password', name: 'email', value: this.state.secret, onChange: this.changeSecret })


                            , React.DOM.button({ className: 'primary button' }, 'Sign in')
                            , error
                            , React.DOM.div
                                ( { className: 'alternative' }
                                , Link
                                    ( { href: '/signup' }
                                    , 'Register Today'
                                    )
                                )
                            )
                        )
                    )
                );
        }

        changeEmail(e) {
            this.setState({ email: e.target.value, secret: this.state.secret, message: this.state.message });
        }

        changeSecret(e) {
            this.setState({ email: this.state.email, secret: e.target.value, message: this.state.message });
        }

        login(e) {
            e.preventDefault();

            this.setState({ email: this.state.email, secret: e.target.value, message: null });
            rmx.signin(rmx.data.session, this.state.email, this.state.secret, () => {
                if (rmx.data.session.status === rmx.SessionStatus.Authenticated) {
                    rmx.data.device.login = this.state.email;
                    rmx.app.navigateTo('/');
                } else {
                    this.setState({ email: this.state.email, secret: '', message: 'failed' });
                }
            });

            return false;
        }
    }

    export var Login = createClass(LoginSpec);
}
