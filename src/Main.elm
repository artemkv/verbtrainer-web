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
    , finalResults : ExerciseFinalResults
    }


type alias ExerciseFinalResults =
    { firstSingular : FinalResult
    , secondSingular : FinalResult
    }


type FinalResult
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
            , finalResults =
                { firstSingular = finalResult state.firstSingular.errorCount
                , secondSingular = finalResult state.secondSingular.errorCount
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
                spec.answers.firstSingular
                state.firstSingular
                FirstSingularChange
            , fillBox
                spec.labels.secondSingular
                spec.answers.secondSingular
                state.secondSingular
                SecondSingularChange
            ]
        ]


type alias OnInputChangeMessageProducer =
    String -> Msg


fillBox : String -> List String -> FillBoxState -> OnInputChangeMessageProducer -> Html Msg
fillBox labelText answers state msg =
    let
        inputClass =
            calculateFillBoxInputClass state.isCompleted (isCorrectSoFar answers state.value)
    in
    div [ class "fill-box-container" ]
        [ div [ class "fill-box-inner1" ]
            []
        , div
            [ class "fill-box-inner2" ]
            [ showHint state.errorCount |> conditionallyPick (hint answers) nothing
            ]
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
                [ class inputClass
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


hint answers =
    div [ class "fill-box-hint" ] [ text (answers |> joined) ]


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
        nothing


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


calculateFillBoxInputClass : Bool -> Bool -> String
calculateFillBoxInputClass isAnswerCompleted isAnswerCorrectSoFar =
    if isAnswerCompleted then
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
                        summary.finalResults.firstSingular
                    , resultBox
                        spec.labels.secondSingular
                        spec.answers.secondSingular
                        summary.finalResults.secondSingular
                    ]
                , div [ class "result-box-inner3" ] []
                ]
            ]
        , div
            []
            [ button [ onClick RetryCompletedExercise ] [ text "Retry" ] -- TODO: move to labels
            ]
        ]


resultBox : String -> List String -> FinalResult -> Html Msg
resultBox labelText answers result =
    div []
        [ span [ class "result-box-correct-form" ]
            [ text (labelText ++ " " ++ (answers |> joined)) ]
        , span
            [ class "result-box-completion" ]
            [ result == Correct |> conditionallyPick completedAndPerfect completedNotPerfect ]
        ]


nothing : Html Msg
nothing =
    span [] []



-- Business


joined : List String -> String
joined =
    String.join "/"


isEqualCI : String -> String -> Bool
isEqualCI a b =
    String.toUpper a == String.toUpper b


startsWithCI : String -> String -> Bool
startsWithCI a b =
    String.toUpper b |> String.startsWith (String.toUpper a)


isCompleted : List String -> String -> Bool
isCompleted expected actual =
    List.any (isEqualCI actual) <| expected


isCorrectSoFar : List String -> String -> Bool
isCorrectSoFar expected actual =
    List.any (startsWithCI actual) <| expected


isPerfect : Int -> Bool
isPerfect errorCount =
    errorCount <= 1


isExerciseCompleted : ExerciseCurrentState -> Bool
isExerciseCompleted state =
    state.firstSingular.isCompleted && state.secondSingular.isCompleted


finalResult : Int -> FinalResult
finalResult =
    isPerfect >> conditionallyPick Correct Incorrect


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


showHint : Int -> Bool
showHint errorCount =
    errorCount > 1
