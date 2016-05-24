/// <reference path="../../data.ts" />
/// <reference path="../../Game.ts" />

/// <reference path="../Base.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../Link.ts" />

module rmx.Component.View {

    export interface SignupState {
        login   : string;
        message : string;
    }

    class SignupSpec extends ReactComponent<{}, SignupState> {
        getInitialState() {
            return { login   : ''
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
                            ( { className: 'login-box form', onSubmit: this.signup }

                            , React.DOM.label({}, 'Pick a username')
                            , React.DOM.input({ className: 'large', type: 'text', name: 'login', value: this.state.login, onChange: this.changeLogin })

                            , React.DOM.button({ className: 'primary button' }, 'Create Account')
                            , error
                            , React.DOM.div
                                ( { className: 'alternative' }
                                , Link
                                    ( { href: '/login' }
                                    , 'Already have an account? Log in here!'
                                    )
                                )
                            )
                        )
                    )
                );
        }

        changeLogin(e) {
            this.setState({ login: e.target.value, message: null });
        }

        signup(e) {
            e.preventDefault();

            this.setState({ login: this.state.login, message: null });
            rmx.signup(rmx.data.session, this.state.login, () => {
                if (rmx.data.session.status === rmx.SessionStatus.Authenticated) {
                    rmx.app.navigateTo('/');
                } else {
                    this.setState({ login: this.state.login, message: 'failed' });
                }
            });

            return false;
        }
    }

    export var Signup = createClass(SignupSpec);
}
