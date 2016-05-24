module rmx.Storage {

    export class Choice {
        id   : string;
        text : string;
    }

    Avers.definePrimitive(Choice, 'text');



    export class SingleChoiceQuestion {
        choices        : Avers.Collection<Choice>;
        selectedChoice : string;
    }

    Avers.defineCollection(SingleChoiceQuestion, 'choices', Choice);
    Avers.definePrimitive(SingleChoiceQuestion, 'selectedChoice');



    export class MultipleChoiceQuestion {
        choices         : Avers.Collection<Choice>;
        selectedChoices : string[];
    }

    Avers.defineCollection(MultipleChoiceQuestion, 'choices', Choice);
    Avers.definePrimitive(MultipleChoiceQuestion, 'selectedChoices', []);



    export type QuestionContent
        = SingleChoiceQuestion
        | MultipleChoiceQuestion
        ;



    export class Question {
        id          : string;
        title       : string;
        description : string;
        content     : QuestionContent;
    }

    var questionTypes =
        { 'single-choice'   : SingleChoiceQuestion
        , 'multiple-choice' : MultipleChoiceQuestion
        };

    Avers.definePrimitive(Question, 'title', '');
    Avers.definePrimitive(Question, 'description', '');
    Avers.defineVariant(Question, 'content', 'type', questionTypes);



    export class Survey {
        createdAt : string;
        questions : Avers.Collection<Question>;
    }

    Avers.definePrimitive(Survey, 'createdAt');
    Avers.defineCollection(Survey, 'questions', Question);
}
