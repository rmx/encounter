/// <reference path="../Base.ts" />
/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Core/Game.ts" />

module rmx.Component.Chrome {

    export interface ObjectiveProps {
        client    : rmx.Game.Client;
        objective : rmx.Pure.Objective;
    }

    class ObjectiveSpec extends ReactComponent<ObjectiveProps, {}> {

        render() {
            var objective   = this.props.objective
              , isCompleted = objective.isCompleted;


            var tasks = rmx.Core.resourceContent<rmx.Storage.Objective>(objective.id).fmap(obj => {
                return obj.tasks.map(task => {
                    return React.DOM.div
                        ( { className: 'task', key: task.id }
                        , React.DOM.span ( { className: 'chrome objective' + (isCompleted ? ' completed' : '') }
                          , taskDescription(task)
                          )
                        );
                });

            }).get(null);


            return React.DOM.div
                ( { className: 'chrome objective container' }
                , tasks
                );
        }

        objectiveName(): string {
            var id = this.props.objective.id;

            return rmx.data.parseReferenceString(id).bind(ref => {
                return rmx.data.displayName(ref);
            }).get(id);
        }
    }

    export var Objective = createClass(ObjectiveSpec);


    function taskDescription(task: rmx.Storage.Task<any>): string {
        if (task.content instanceof rmx.Storage.KillCreature) {
            return rmx.data.resolveReference<rmx.Storage.Resource<rmx.Storage.Creature>>(task.content.creature).fmap(creature => {
                return "Kill " + task.content.count + " of " + creature.content.name;
            }).get('...');

        } else if (task.content instanceof rmx.Storage.KillParties) {
            return "Kill other parties";

        } else {
            return "???";
        }
    }
}
