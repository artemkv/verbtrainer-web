module VerbConjugator exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Monocle.Compose as Compose
import Monocle.Lens exposing (Lens)
import Util exposing (conditionallyPick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    VerbConjugationExercise


type alias VerbConjugationExercise =
    { spec : ExerciseSpec
    , currentState : ExerciseCurrentState
    }


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


init : Model
init =
    -- TODO: load spec from JSON
    { spec =
        { labels =
            { firstSingular = "label1"
            , secondSingular = "label2"
            }
        , answers =
            { firstSingular = [ "hello" ]
            , secondSingular = [ "world", "you" ]
            }
        }
    , currentState =
        { firstSingular = emptyFillBoxState
        , secondSingular = emptyFillBoxState
        }
    }



-- Lens


currentStateOfExercise : Lens VerbConjugationExercise ExerciseCurrentState
currentStateOfExercise =
    Lens .currentState (\b a -> { a | currentState = b })


firstSingularFillBoxStateOfExerciseCurrentState : Lens ExerciseCurrentState FillBoxState
firstSingularFillBoxStateOfExerciseCurrentState =
    Lens .firstSingular (\b a -> { a | firstSingular = b })


secondSingularFillBoxStateOfExerciseCurrentState : Lens ExerciseCurrentState FillBoxState
secondSingularFillBoxStateOfExerciseCurrentState =
    Lens .secondSingular (\b a -> { a | secondSingular = b })


firstSingularFillBoxStateOfExercise : Lens VerbConjugationExercise FillBoxState
firstSingularFillBoxStateOfExercise =
    currentStateOfExercise
        |> Compose.lensWithLens firstSingularFillBoxStateOfExerciseCurrentState


secondSingularFillBoxStateOfExercise : Lens VerbConjugationExercise FillBoxState
secondSingularFillBoxStateOfExercise =
    currentStateOfExercise
        |> Compose.lensWithLens secondSingularFillBoxStateOfExerciseCurrentState



-- Update


type Msg
    = FirstSingularChange String
    | SecondSingularChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstSingularChange newValue ->
            let
                ( newIsCompleted, newErrorCount ) =
                    updateFillBoxState model.spec.answers.firstSingular newValue model.currentState.firstSingular
            in
            model
                |> firstSingularFillBoxStateOfExercise.set (FillBoxState newValue newIsCompleted newErrorCount)

        SecondSingularChange newValue ->
            let
                ( newIsCompleted, newErrorCount ) =
                    updateFillBoxState model.spec.answers.secondSingular newValue model.currentState.secondSingular
            in
            model
                |> secondSingularFillBoxStateOfExercise.set (FillBoxState newValue newIsCompleted newErrorCount)


updateFillBoxState : List String -> String -> FillBoxState -> ( Bool, Int )
updateFillBoxState answers newValue state =
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
    div []
        [ fillBox
            model.spec.labels.firstSingular
            model.currentState.firstSingular
            FirstSingularChange
        , fillBox
            model.spec.labels.secondSingular
            model.currentState.secondSingular
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
