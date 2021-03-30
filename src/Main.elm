module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Util exposing (conditionallyPick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type Model
    = ExerciseInProgress ExerciseSpec ExerciseCurrentState
    | ExerciseCompleted ExerciseSpec ExerciseSummary
    | Error String


type alias ExerciseSpec =
    { labels : ExerciseLabels
    , answers : ExerciseAnswers
    }


type alias ExerciseLabels =
    { firstSingular : String
    , secondSingular : String
    }


type alias ExerciseAnswers =
    { firstSingular : List String
    , secondSingular : List String
    }


type alias ExerciseCurrentState =
    { firstSingular : FillBoxState
    , secondSingular : FillBoxState
    }


type alias FillBoxState =
    { value : String
    , isCompleted : Bool
    , errorCount : Int
    }


emptyFillBoxState : FillBoxState
emptyFillBoxState =
    { value = ""
    , isCompleted = False
    , errorCount = 0
    }


type alias ExerciseSummary =
    { overallResult : String
    , feedback : String
    , individualResults : ExerciseResults
    }


type alias ExerciseResults =
    { firstSingular : ExerciseResult
    , secondSingular : ExerciseResult
    }


type ExerciseResult
    = Correct
    | Incorrect


init : Model
init =
    -- TODO: load spec from JSON
    ExerciseInProgress
        { labels =
            { firstSingular = "Yo"
            , secondSingular = "Tú"
            }
        , answers =
            { firstSingular = [ "hablo" ]
            , secondSingular = [ "hablas" ]
            }
        }
        { firstSingular = emptyFillBoxState
        , secondSingular = emptyFillBoxState
        }


type Msg
    = FirstSingularChange String
    | SecondSingularChange String
    | RetryCompletedExercise


update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstSingularChange _ ->
            updateExerciseInProgress msg model

        SecondSingularChange _ ->
            updateExerciseInProgress msg model

        RetryCompletedExercise ->
            init


updateExerciseInProgress : Msg -> Model -> Model
updateExerciseInProgress msg model =
    case model of
        ExerciseInProgress spec state ->
            case msg of
                FirstSingularChange newValue ->
                    let
                        ( newIsCompleted, newErrorCount ) =
                            getNewFillBoxStateValues spec.answers.firstSingular newValue state.firstSingular
                    in
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | firstSingular = FillBoxState newValue newIsCompleted newErrorCount }

                SecondSingularChange newValue ->
                    let
                        ( newIsCompleted, newErrorCount ) =
                            getNewFillBoxStateValues spec.answers.secondSingular newValue state.secondSingular
                    in
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | secondSingular = FillBoxState newValue newIsCompleted newErrorCount }

                _ ->
                    Error "Invalid message for exercise in progress"

        _ ->
            Error "Message is intended for exercise in progress"


returnAsExerciseInProgressOrCompleted : ExerciseSpec -> ExerciseCurrentState -> Model
returnAsExerciseInProgressOrCompleted spec state =
    if isExerciseCompleted state then
        let
            incorrectTotal =
                (isPerfect state.firstSingular.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.secondSingular.errorCount |> conditionallyPick 0 1)

            ( overallResult, feedback ) =
                getExerciseOverallResultAndFeedback incorrectTotal
        in
        ExerciseCompleted spec
            { overallResult = overallResult
            , feedback = feedback
            , individualResults =
                { firstSingular = exerciseResult state.firstSingular.errorCount
                , secondSingular = exerciseResult state.secondSingular.errorCount
                }
            }

    else
        ExerciseInProgress spec state


getNewFillBoxStateValues : List String -> String -> FillBoxState -> ( Bool, Int )
getNewFillBoxStateValues answers newValue state =
    let
        newIsCompleted : Bool
        newIsCompleted =
            isCompleted answers newValue

        isCorrectAnswer : String -> Bool
        isCorrectAnswer =
            isCorrectSoFar answers

        wasCorrect : Bool
        wasCorrect =
            isCorrectAnswer state.value

        isNotCorrectAnymore : Bool
        isNotCorrectAnymore =
            isCorrectAnswer newValue |> not

        errorDelta : Int
        errorDelta =
            wasCorrect && isNotCorrectAnymore |> conditionallyPick 1 0

        newErrorCount : Int
        newErrorCount =
            state.errorCount + errorDelta
    in
    ( newIsCompleted, newErrorCount )



-- View


view : Model -> Html Msg
view model =
    case model of
        ExerciseInProgress spec state ->
            verbConjugator spec state

        ExerciseCompleted spec summary ->
            verbConjugatorCompletionScore spec summary

        Error errorText ->
            div [] [ text errorText ]


verbConjugator : ExerciseSpec -> ExerciseCurrentState -> Html Msg
verbConjugator spec state =
    div []
        [ div [ class "verb-conjugator" ]
            [ div [ class "verb-conjugator-verb" ] [ text "Verb" ]
            , div [ class "verb-conjugator-tense" ] [ text "tense" ]
            , fillBox
                spec.labels.firstSingular
                state.firstSingular
                (isCorrectSoFar spec.answers.firstSingular state.firstSingular.value)
                FirstSingularChange
            , fillBox
                spec.labels.secondSingular
                state.secondSingular
                (isCorrectSoFar spec.answers.secondSingular state.secondSingular.value)
                SecondSingularChange
            ]
        ]


type alias OnInputChangeMessageProducer =
    String -> Msg


fillBox : String -> FillBoxState -> Bool -> OnInputChangeMessageProducer -> Html Msg
fillBox labelText state isAnswerCorrectSoFar msg =
    div [ class "fill-box-container" ]
        [ div [ class "fill-box-inner1" ]
            []
        , div
            [ class "fill-box-inner2" ]
            []
        , div
            [ class "fill-box-inner3" ]
            []
        , div
            [ class "fill-box-inner4" ]
            [ label [ class "fill-box-label" ] [ text labelText ]
            ]
        , div
            [ class "fill-box-inner5" ]
            [ input
                [ class (calculateFillBoxInputClass state isAnswerCorrectSoFar)
                , type_ "text"
                , value state.value
                , onInput msg
                ]
                []
            ]
        , div
            [ class "fill-box-inner6" ]
            [ span [ class "fill-box-completion" ] [ completionSign state ] ]
        ]


completionSign : FillBoxState -> Html Msg
completionSign state =
    let
        isAnswerPerfect =
            isPerfect state.errorCount
    in
    if state.isCompleted && isAnswerPerfect then
        completedAndPerfect

    else if not isAnswerPerfect then
        completedNotPerfect

    else
        span [] []


completedAndPerfect : Html Msg
completedAndPerfect =
    span [ class "completed-perfect-mark" ]
        [ img
            [ class "completed-perfect-image"
            , src "./correct.png"
            ]
            []
        ]


completedNotPerfect : Html Msg
completedNotPerfect =
    span [ class "completed-imperfect-mark" ]
        [ img
            [ class "completed-imperfect-image"
            , src "./incorrect.png"
            ]
            []
        ]


calculateFillBoxInputClass : FillBoxState -> Bool -> String
calculateFillBoxInputClass state isAnswerCorrectSoFar =
    if state.isCompleted then
        "fill-box-input completed"

    else if not isAnswerCorrectSoFar then
        "fill-box-input incorrect"

    else
        "fill-box-input"


verbConjugatorCompletionScore : ExerciseSpec -> ExerciseSummary -> Html Msg
verbConjugatorCompletionScore spec summary =
    div []
        [ div [ class "exercise-completion-score" ]
            [ div [ class "exercise-completion-score-result1" ] [ text summary.overallResult ]
            , div [ class "exercise-completion-score-result2" ] [ text summary.feedback ]
            , div [ class "exercise-completion-results" ]
                [ div [ class "result-box-inner1" ] []
                , div [ class "result-box-inner2" ]
                    [ resultBox
                        spec.labels.firstSingular
                        spec.answers.firstSingular
                        summary.individualResults.firstSingular
                    , resultBox
                        spec.labels.secondSingular
                        spec.answers.secondSingular
                        summary.individualResults.secondSingular
                    ]
                , div [ class "result-box-inner3" ] []
                ]
            ]
        , div
            []
            [ button [ onClick RetryCompletedExercise ] [ text "Retry" ] -- TODO: move to labels
            ]
        ]


resultBox : String -> List String -> ExerciseResult -> Html Msg
resultBox labelText answers result =
    div []
        [ span [ class "result-box-correct-form" ]
            [ text (labelText ++ " " ++ (answers |> String.join "/")) ]
        , span
            [ class "result-box-completion" ]
            [ result == Correct |> conditionallyPick completedAndPerfect completedNotPerfect ]
        ]


isCompleted : List String -> String -> Bool
isCompleted expected actual =
    -- TODO: use locale-specific accent-sensitive compare
    -- TODO: unit-test
    expected |> List.any (\x -> x == actual)


isCorrectSoFar : List String -> String -> Bool
isCorrectSoFar expected actual =
    -- TODO: use locale-specific accent-sensitive compare
    -- TODO: unit-test
    expected |> List.any (\x -> x |> String.startsWith actual)


isPerfect : Int -> Bool
isPerfect errorCount =
    errorCount <= 1


isExerciseCompleted : ExerciseCurrentState -> Bool
isExerciseCompleted state =
    state.firstSingular.isCompleted && state.secondSingular.isCompleted


exerciseResult : Int -> ExerciseResult
exerciseResult errorCount =
    isPerfect errorCount |> conditionallyPick Correct Incorrect


getExerciseOverallResultAndFeedback : Int -> ( String, String )
getExerciseOverallResultAndFeedback exerciseIncorrectTotal =
    if exerciseIncorrectTotal == 0 then
        ( "Perfect score!", "Great job!" )

    else if exerciseIncorrectTotal == 1 then
        ( "Well done!", "Almost there!" )

    else if exerciseIncorrectTotal <= 4 then
        ( "Keep practicing!", "You'll get there!" )

    else
        ( "Don't give up!", "You will do better next time!" )
