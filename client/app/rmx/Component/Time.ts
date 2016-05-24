/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

module rmx.Component {

    export interface TimeProps {
        datetime : string;
    }

    export interface TimeState {
        timerId : any;
    }

    class TimeSpec extends ReactComponent<TimeProps, TimeState> {

        getInitialState() {
            return { timerId: null };
        }

        render() {
            try {
                // XXX
                var words = (<any>window).dateInWords(new Date(Date.parse(this.props.datetime)));
            } catch (e) {
                console.log(e);
            }

            return React.DOM.time({ dateTime: this.props.datetime }, words);
        }

        componentDidMount() {
            var refresh = this.setState.bind(this, {});
            this.setState({ timerId: setInterval(refresh, 60000) });
        }

        componentWillUnmount() {
            clearInterval(this.state.timerId);
        }
    }

    export var Time = createClass(TimeSpec);
}
