module rmx.Survey {

    export class QuestionTemplate {
        constructor
          ( public title       : string
          , public description : string
          , public content     : QuestionTemplateContent
          ) {}
    }

    export type QuestionTemplateContent
        = SingleChoiceQuestionTemplate
        | MultipleChoiceQuestionTemplate
        ;

    export class SingleChoiceQuestionTemplate {
        constructor
          ( public choices : Choice[]
          ) {}
    }

    export class MultipleChoiceQuestionTemplate {
        constructor
          ( public choices : Choice[]
          ) {}
    }

    export class Choice {
        constructor
          ( public text : string
          ) {}
    }

    export var questions =
        [ new QuestionTemplate
            ( 'Do you like this type of game?'
            , 'Third person view, fantasy world, spells, wizzards and warriors. Mechanic a bit like D&D. Short games (max 10 minutes), no character development.'
            , new SingleChoiceQuestionTemplate
                ( [ new Choice('Yes')
                  , new Choice('No')
                  ]
                )
            )

        , new QuestionTemplate
            ( 'Was this tutorial helpful?'
            , 'Do you feel like you know what this game is about? Or is there something you\'d like to have covered in a tutorial but that wasn\'t shown here?'
            , new SingleChoiceQuestionTemplate
                ( [ new Choice('It was rubbish')
                  , new Choice('To a certain degree, though something was missing')
                  , new Choice('Yes, I know everything about the game that I wanted to know')
                  ]
                )
            )

        , new QuestionTemplate
            ( 'How much would such a game be worth to you?'
            , 'If we were to charge for this game, hypothetically speaking, how much would you be willing to pay per month for the priviledge to play an unlimited number of games.'
            , new SingleChoiceQuestionTemplate
                ( [ new Choice('Less than 1$')
                  , new Choice('1$ - 5$')
                  , new Choice('5$ - 15$')
                  , new Choice('15$ - 30$')
                  , new Choice('30$ or more')
                  ]
                )
            )

        , new QuestionTemplate
            ( 'What should we improve?'
            , 'We know that the game is far from perfect. What should we concentrate on in the immediate future? What do you think would improve the experience the most at this stage?'
            , new MultipleChoiceQuestionTemplate
                ( [ new Choice('Better graphics.')
                  , new Choice('How about a bit music and more sound effects in the game?')
                  , new Choice('The controls are clunky.')
                  , new Choice('Moar encounters, I\'ve finished the few which you have and now have nothing left to do.')
                  ]
                )
            )

        , new QuestionTemplate
            ( 'Do you want to create your own encounter?'
            , null
            , new SingleChoiceQuestionTemplate
                ( [ new Choice('Yes! Where, how, tell me now!')
                  , new Choice('No, I\'ll rather just play the games.')
                  ]
                )
            )
        ];
}
