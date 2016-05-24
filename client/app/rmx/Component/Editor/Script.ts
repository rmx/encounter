/// <reference path="../../data.ts" />

/// <reference path="../Base.ts" />


declare var ace: any;

module rmx.Component.Editor {

    export interface ScriptState {
        session;
    }

    export interface ScriptProps {
        object : any;
        field  : string;
    }

    class ScriptSpec extends ReactComponent<ScriptProps, ScriptState> {

        getInitialState() {
            return { session: ace.createEditSession('', 'ace/mode/text') };
        }

        render() {
            var value = this.props.object[this.props.field];
                if (this.state.session.getValue() !== value) {
                    this.state.session.setValue(value);
                }

                return React.DOM.div({ ref: 'script', style: { flex: 1 } });
        }

        componentDidMount() {
            var editor = ace.edit(this.refs['script'].getDOMNode());
            editor.setSession(this.state.session);
            var mode = ace.require('ace/mode/coffee');
            this.state.session.setMode(new mode.Mode());
            //editor.setKeyboardHandler(ace.require('ace/keyboard/vim').handler);

            editor.getSession().on('change', function(e) {
                var value = editor.getSession().getValue();
                this.props.object[this.props.field] = value;
            }.bind(this));
        }
    }

    export var Script = createClass(ScriptSpec);
}
