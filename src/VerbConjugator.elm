module VerbConjugator exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, input, label, span, text)
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
                    Error ("Invalid message " ++ toString msg ++ " for exercise in progress")

        _ ->
            Error ("Message " ++ toString msg ++ " is intended for exercise in progress")


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
        [ fillBox
            spec.labels.firstSingular
            state.firstSingular
            FirstSingularChange
        , fillBox
            spec.labels.secondSingular
            state.secondSingular
            SecondSingularChange
        ]


type alias OnInputChangeMessageProducer =
    String -> Msg


fillBox : String -> FillBoxState -> OnInputChangeMessageProducer -> Html Msg
fillBox labelText currentState msg =
    div []
        [ label [ class "fill-box-label" ]
            [ text labelText
            , input [ value currentState.value, onInput msg ] []
            ]
        , text
            (if currentState.isCompleted then
                currentState.errorCount
                    |> isPerfect
                    |> conditionallyPick " V | " " X | "

             else
                " - | "
            )
        , text ("errors: " ++ (currentState.errorCount |> toString))
        ]


verbConjugatorCompletionScore : ExerciseSpec -> ExerciseSummary -> Html Msg
verbConjugatorCompletionScore spec summary =
    div []
        [ div []
            [ div [] [ text summary.overallResult ]
            , div [] [ text summary.feedback ]
            , div []
                [ resultBox
                    spec.labels.firstSingular
                    spec.answers.firstSingular
                    summary.individualResults.firstSingular
                , resultBox
                    spec.labels.secondSingular
                    spec.answers.secondSingular
                    summary.individualResults.secondSingular
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
        [ span []
            [ text (labelText ++ " " ++ (answers |> String.join "/")) ]
        , span
            []
            [ text (result == Correct |> conditionallyPick " V " " X ") ]
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
