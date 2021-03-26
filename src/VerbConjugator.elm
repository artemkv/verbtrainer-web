module VerbConjugator exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Util exposing (conditionallyPick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type Model
    = ExerciseInProgress ExerciseSpec ExerciseCurrentState
    | ExerciseCompleted ExerciseSpec ExerciseSummary


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
            { firstSingular = "label1"
            , secondSingular = "label2"
            }
        , answers =
            { firstSingular = [ "hello" ]
            , secondSingular = [ "world", "you" ]
            }
        }
        { firstSingular = emptyFillBoxState
        , secondSingular = emptyFillBoxState
        }


type Msg
    = FirstSingularChange String
    | SecondSingularChange String


update : Msg -> Model -> Model
update msg model =
    case model of
        ExerciseInProgress spec state ->
            updateExerciseInProgress msg spec state

        ExerciseCompleted spec summary ->
            updateExerciseCompleted msg spec summary


updateExerciseInProgress : Msg -> ExerciseSpec -> ExerciseCurrentState -> Model
updateExerciseInProgress msg spec state =
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


returnAsExerciseInProgressOrCompleted : ExerciseSpec -> ExerciseCurrentState -> Model
returnAsExerciseInProgressOrCompleted spec newState =
    let
        isExerciseCompleted =
            newState.firstSingular.isCompleted && newState.secondSingular.isCompleted
    in
    if isExerciseCompleted then
        -- TODO
        ExerciseCompleted spec
            { overallResult = "DONE"
            , feedback = "GOOD"
            , individualResults =
                { firstSingular = Correct
                , secondSingular = Correct
                }
            }

    else
        ExerciseInProgress spec newState


updateExerciseCompleted : Msg -> ExerciseSpec -> ExerciseSummary -> Model
updateExerciseCompleted msg spec summary =
    ExerciseCompleted spec summary


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
            div [] [ text "Is completed!" ]


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



-- Business


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


isPerfect : number -> Bool
isPerfect errorCount =
    errorCount <= 1
