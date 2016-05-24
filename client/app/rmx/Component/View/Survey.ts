/// <reference path="../Base.ts" />
/// <reference path="../NavBar.ts" />
/// <reference path="../Link.ts" />
/// <reference path="../Body.ts" />
/// <reference path="../Site.ts" />
/// <reference path="../Main.ts" />
/// <reference path="../MainNav.ts" />
/// <reference path="../AppNavBar.ts" />
/// <reference path="../DrawerCloser.ts" />
/// <reference path="../GameSetupSiteHeader.ts" />

/// <reference path="../../data.ts" />
/// <reference path="../../Game/Client.ts" />
/// <reference path="../../Core/Game.ts" />
/// <reference path="../../Storage/Survey.ts" />
/// <reference path="../../Survey.ts" />


module rmx.Component.View {

    export interface SurveyState {
        questionIndex : number;
        survey        : rmx.Storage.Survey;
    }

    class SurveySpec extends ReactComponent<{}, SurveyState> {

        getInitialState() {
            var createdAt = new Date().toISOString()
              , survey    = Avers.mk(rmx.Storage.Survey, { createdAt: createdAt });

            // Instantiate all questions into the survey.
            rmx.Survey.questions.forEach(q => {
                var question = Avers.parseJSON(rmx.Storage.Question, {});
                rmx.data.pushItem(survey.questions, question);

                question.title       = q.title;
                question.description = q.description;
                question.content     = instantiateQuestionContent(q.content);
            });

            return { questionIndex : 0
                   , survey        : survey
                   };
        }

        render() {
            var survey   = this.state.survey
              , question = survey.questions[this.state.questionIndex];

            if (question) {
                return Body
                    ( {}
                    , Main
                        ( {}
                        , renderQuestion(this, question)
                        )
                    );

            } else {
                return GameAfterTutorial();
            }
        }

        answerQuestion(q) {
            this.setState(
                { questionIndex : this.state.questionIndex + 1
                , survey        : this.state.survey
                }
            );

            if (this.state.questionIndex + 1 >= this.state.survey.questions.length) {
                // All questions answered. Submit the result to the server.
                sendSurvey(this.state.survey);
            }
        }
    }

    export var Survey = createClass(SurveySpec);

    function sendSurvey(survey: rmx.Storage.Survey): void {
        var xhr = new XMLHttpRequest();
        xhr.withCredentials = true;

        xhr.open('POST', rmx.apiUrl('/surveys'));
        xhr.setRequestHeader('Content-Type', 'application/json');
        xhr.send(JSON.stringify({ survey: Avers.toJSON(survey) }));

        xhr.addEventListener('readystatechange', onReadyStateChange, false);
        function onReadyStateChange() {
            if (xhr.readyState === 4) {
                if (xhr.status === 200) {
                    console.log('sendSurvey: ok', xhr.responseText);
                } else {
                    console.log('sendSurvey: failed', xhr.responseText);
                }
            }
        }
    }



    function renderQuestion
    ( c        : SurveySpec
    , question : rmx.Survey.QuestionTemplate
    ): any {
        if (question.content instanceof rmx.Storage.SingleChoiceQuestion) {
            return singleChoiceQuestion(c, question, question.content);

        } else if (question.content instanceof rmx.Storage.MultipleChoiceQuestion) {
            return multipleChoiceQuestion(c, question, question.content);
        }
    }


    function singleChoiceQuestion(c, question, content) {
        var choices = content.choices.map(choice => {
            function onClick() {
                content.selectedChoice = choice.id;
                c.setState({});
            }

            var className = 'choice';
            if (content.selectedChoice === choice.id) {
                className += ' selected';
            }

            return React.DOM.div
                ( { onClick: onClick, className: className }
                , choice.text
                );
        });

        return React.DOM.div
            ( { className: 'rmx survey-question-content' }
            , React.DOM.h1({}, question.title)
            , React.DOM.p({}, question.description)
            , React.DOM.div
                ( { className: 'choices' }
                , choices
                )
            , buttonBar(c, !!content.selectedChoice, submitFn(c, question))
            );
    }

    function submitFn(c, question) {
        return function() {
            c.answerQuestion(question);
        };
    }

    function multipleChoiceQuestion(c, question, content) {
        var choices = content.choices.map(choice => {
            function onClick() {
                if (content.selectedChoices.indexOf(choice.id) !== -1) {
                    content.selectedChoices = content.selectedChoices.filter(x => {
                        return x !== choice.id;
                    });
                } else {
                    content.selectedChoices.push(choice.id);
                    content.selectedChoices = _.uniq(content.selectedChoices);
                }

                c.setState({});
            }

            var className = 'choice';
            if (content.selectedChoices.indexOf(choice.id) !== -1) {
                className += ' selected';
            }

            return React.DOM.div
                ( { onClick: onClick, className: className }
                , choice.text
                );
        });

        return React.DOM.div
            ( { className: 'rmx survey-question-content' }
            , React.DOM.h1({}, question.title)
            , React.DOM.p({}, question.description)
            , React.DOM.div
                ( { className: 'choices' }
                , choices
                )
            , buttonBar(c, true, submitFn(c, question))
            );
    }


    function
    instantiateQuestionContent
    ( content : rmx.Survey.QuestionTemplateContent
    ): rmx.Storage.QuestionContent {
        var ret;

        if (content instanceof rmx.Survey.SingleChoiceQuestionTemplate) {
            ret = Avers.mk(rmx.Storage.SingleChoiceQuestion, {});
            content.choices.forEach(choice => {
                rmx.data.pushItem
                    ( ret.choices
                    , Avers.mk(rmx.Storage.Choice, { text: choice.text })
                    );
            });

        } else if (content instanceof rmx.Survey.MultipleChoiceQuestionTemplate) {
            ret = Avers.mk(rmx.Storage.MultipleChoiceQuestion, {});
            content.choices.forEach(choice => {
                rmx.data.pushItem
                    ( ret.choices
                    , Avers.mk(rmx.Storage.Choice, { text: choice.text })
                    );
            });
        }

        return ret;
    }

    function buttonBar(surveyComponent, showNextButton, onNext) {
        var nextButton = null;
        if (showNextButton) {
            nextButton = React.DOM.div
                ( { className: 'large primary button', onClick: onNext }
                , 'Next'
                );
        }

        function skipSurvey() {
            surveyComponent.setState(
                { questionIndex : surveyComponent.state.questionIndex + 999
                , survey        : surveyComponent.state.survey
                }
            );
        }

        return React.DOM.div
            ( { style: { display: 'flex', padding: '1rem' } }
            , React.DOM.div
                ( { className: 'large button', onClick: skipSurvey }
                , 'Exit'
                )
            , React.DOM.div({ style: { flex: 1 } })
            , nextButton
            );
    }
}
