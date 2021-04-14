port module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, img, input, label, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, bool, decodeString, field, map2, map6, string)
import Url
import Url.Parser exposing ((</>), Parser)
import Util exposing (conditionallyPick)



-- Model


type alias Model =
    { navKey : Nav.Key
    , appModel : AppModel
    }


type AppModel
    = ExerciseNotLoaded
    | ExerciseLoadingFailed String
    | ExerciseInProgress ExerciseSpec ExerciseCurrentState
    | ExerciseCompleted ExerciseSpec ExerciseSummary
    | Error ErrorDetails


type ErrorDetails
    = IncompatibleMessageForState Msg AppModel
    | Other String


type alias ExerciseId =
    String


type alias ExerciseSpec =
    { id : ExerciseId
    , verb : String
    , tense : String
    , labels : ExerciseLabels
    , answers : ExerciseAnswers
    , next : NextExerciseReference
    }


type alias ExerciseLabels =
    { firstSingular : String
    , secondSingular : String
    }


type alias ExerciseAnswers =
    { firstSingular : List String
    , secondSingular : List String
    }


type alias NextExerciseReference =
    { id : ExerciseId
    , verb : String
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


emptyExerciseState : ExerciseCurrentState
emptyExerciseState =
    { firstSingular = emptyFillBoxState
    , secondSingular = emptyFillBoxState
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    handleRouteChange url (Model navKey ExerciseNotLoaded)



-- Update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ExerciseDataReceived String
    | FirstSingularChange String
    | SecondSingularChange String
    | RetryCompletedExercise
    | MoveToExercise ExerciseId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        appModel =
            model.appModel
    in
    case msg of
        LinkClicked urlRequest ->
            navigate urlRequest model

        UrlChanged url ->
            handleRouteChange url model

        MoveToExercise id ->
            navigateToExercise id model

        ExerciseDataReceived data ->
            updateExerciseFromReceivedData data msg appModel |> asNewAppModelOf model |> justModel

        FirstSingularChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelOf model |> justModel

        SecondSingularChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelOf model |> justModel

        RetryCompletedExercise ->
            clearExerciseState msg appModel |> asNewAppModelOf model |> justModel


navigate : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
navigate urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


handleRouteChange : Url.Url -> Model -> ( Model, Cmd Msg )
handleRouteChange url model =
    case toRoute url of
        -- TODO: handle home correctly
        Home ->
            ( ExerciseNotLoaded |> asNewAppModelOf model, requestExerciseData "hablar" )

        Exercise id ->
            ( ExerciseNotLoaded |> asNewAppModelOf model, requestExerciseData id )

        -- TODO: handle not found correctly
        NotFound ->
            ( model, Cmd.none )


navigateToExercise : ExerciseId -> Model -> ( Model, Cmd Msg )
navigateToExercise id model =
    ( model, Nav.pushUrl model.navKey (getExerciseLink id) )


asNewAppModelOf : Model -> AppModel -> Model
asNewAppModelOf model appModel =
    { model | appModel = appModel }


justModel : Model -> ( Model, Cmd Msg )
justModel model =
    ( model, Cmd.none )


updateExerciseFromReceivedData : String -> Msg -> AppModel -> AppModel
updateExerciseFromReceivedData data msg model =
    case model of
        ExerciseNotLoaded ->
            processReceivedExerciseData data

        _ ->
            IncompatibleMessageForState msg model |> Error


updateExerciseInProgress : Msg -> AppModel -> AppModel
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
                    IncompatibleMessageForState msg model |> Error

        _ ->
            IncompatibleMessageForState msg model |> Error


returnAsExerciseInProgressOrCompleted : ExerciseSpec -> ExerciseCurrentState -> AppModel
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

        isCorrectAnswerSoFar : String -> Bool
        isCorrectAnswerSoFar =
            isCorrectSoFar answers

        wasCorrect : Bool
        wasCorrect =
            isCorrectAnswerSoFar state.value

        isNotCorrectAnymore : Bool
        isNotCorrectAnymore =
            isCorrectAnswerSoFar newValue |> not

        errorDelta : Int
        errorDelta =
            wasCorrect && isNotCorrectAnymore |> conditionallyPick 1 0

        newErrorCount : Int
        newErrorCount =
            state.errorCount + errorDelta
    in
    ( newIsCompleted, newErrorCount )


clearExerciseState : Msg -> AppModel -> AppModel
clearExerciseState msg model =
    case model of
        ExerciseCompleted spec _ ->
            ExerciseInProgress spec emptyExerciseState

        _ ->
            IncompatibleMessageForState msg model |> Error



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    exerciseDataReceived ExerciseDataReceived



-- View


view : Model -> Browser.Document Msg
view model =
    -- TODO: where should the title come from?
    { title = "Verb Conjugation Exercises"
    , body = [ body model.appModel ]
    }


body : AppModel -> Html Msg
body model =
    case model of
        ExerciseNotLoaded ->
            -- TODO: render spinner
            div [] [ text "Loading..." ]

        ExerciseLoadingFailed reason ->
            -- TODO: render error correctly
            div [] [ text reason ]

        ExerciseInProgress spec state ->
            verbConjugator spec state

        ExerciseCompleted spec summary ->
            verbConjugatorCompletionScore spec summary

        Error errorDetails ->
            div [] [ text (errorDetails |> errorText) ]


errorText : ErrorDetails -> String
errorText err =
    case err of
        IncompatibleMessageForState _ _ ->
            -- TODO: how to print out msg/model?
            "Message is incompatible with the current state of the model"

        Other s ->
            s


verbConjugator : ExerciseSpec -> ExerciseCurrentState -> Html Msg
verbConjugator spec state =
    div []
        [ div [ class "verb-conjugator" ]
            [ div [ class "verb-conjugator-verb" ] [ text spec.verb ]
            , div [ class "verb-conjugator-tense" ] [ text spec.tense ]
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
        , nextExerciseReference spec.next
        ]


type alias OnInputChangeMessageProducer =
    String -> Msg


fillBox : String -> List String -> FillBoxState -> OnInputChangeMessageProducer -> Html Msg
fillBox labelText answers state msg =
    let
        isAnswerCorrectSoFar =
            isCorrectSoFar answers state.value

        inputClass =
            calculateFillBoxInputClass state.isCompleted isAnswerCorrectSoFar
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


hint : List String -> Html msg
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
            , src "/correct.png"
            ]
            []
        ]


completedNotPerfect : Html Msg
completedNotPerfect =
    span [ class "completed-imperfect-mark" ]
        [ img
            [ class "completed-imperfect-image"
            , src "/incorrect.png"
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


nextExerciseReference : NextExerciseReference -> Html Msg
nextExerciseReference next =
    div [ class "verb-conjugator-next" ]
        [ div [ class "verb-conjugator-next-inner1" ] []
        , div [ class "verb-conjugator-next-inner2" ]
            [ a
                [ class "verb-conjugator-next-link"
                , href (getExerciseLink next.id)
                ]
                [ text (next.verb ++ " >") ]
            ]
        , div [ class "verb-conjugator-next-inner3" ] []
        ]


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
        , div [ class "exercise-completion-score-next" ]
            [ div [ class "exercise-completion-score-next-inner1" ] []
            , div [ class "exercise-completion-score-next-inner2" ]
                [ div [ class "exercise-completion-score-buttons" ]
                    [ button
                        [ class "exercise-completion-score-retry-button"
                        , onClick RetryCompletedExercise
                        ]
                        -- TODO: move to labels
                        [ text "Retry" ]
                    , button
                        [ class "exercise-completion-score-next-button"
                        , onClick (MoveToExercise spec.next.id)
                        ]
                        -- TODO: move to labels
                        [ text (spec.next.verb ++ " >") ]
                    ]
                ]
            , div [ class "exercise-completion-score-next-inner3" ] []
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
    -- TODO: move to labels
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



-- Data loading


port requestExerciseData : String -> Cmd id


port exerciseDataReceived : (String -> msg) -> Sub msg


processReceivedExerciseData : String -> AppModel
processReceivedExerciseData data =
    let
        isOk =
            data |> decodeString (field "isOk" bool)
    in
    case isOk of
        Ok success ->
            if success then
                decodeExerciseData data

            else
                decodeExerciseLoadingErrorData data

        Err err ->
            ExerciseLoadingFailed (Json.Decode.errorToString err)


decodeExerciseData : String -> AppModel
decodeExerciseData data =
    let
        result =
            data |> decodeString exerciseSpecDecoder
    in
    case result of
        Ok spec ->
            ExerciseInProgress spec emptyExerciseState

        Err err ->
            ExerciseLoadingFailed (Json.Decode.errorToString err)


decodeExerciseLoadingErrorData : String -> AppModel
decodeExerciseLoadingErrorData data =
    let
        result =
            data |> decodeString exerciseLoadingErrorDecoder
    in
    case result of
        Ok err ->
            ExerciseLoadingFailed err

        Err err ->
            ExerciseLoadingFailed (Json.Decode.errorToString err)


exerciseSpecDecoder : Decoder ExerciseSpec
exerciseSpecDecoder =
    field "data"
        (map6 ExerciseSpec
            (field "id" string)
            (field "verb" string)
            (field "tense" string)
            (field "labels" exerciseLabelsDecoder)
            (field "answers" exerciseAnswersDecoder)
            (field "next" exerciseNextDecoder)
        )


exerciseLabelsDecoder : Decoder ExerciseLabels
exerciseLabelsDecoder =
    map2 ExerciseLabels
        (field "firstSingular" string)
        (field "secondSingular" string)


exerciseAnswersDecoder : Decoder ExerciseAnswers
exerciseAnswersDecoder =
    map2 ExerciseAnswers
        (field "firstSingular" (Json.Decode.list string))
        (field "secondSingular" (Json.Decode.list string))


exerciseNextDecoder : Decoder NextExerciseReference
exerciseNextDecoder =
    map2 NextExerciseReference
        (field "id" string)
        (field "verb" string)


exerciseLoadingErrorDecoder : Decoder String
exerciseLoadingErrorDecoder =
    field "data" (field "err" string)



-- Routing


type Route
    = Home
    | Exercise ExerciseId
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Exercise (Url.Parser.s "exercise" </> Url.Parser.string)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Url.Parser.parse routeParser url |> Maybe.withDefault NotFound


getExerciseLink id =
    "/exercise/" ++ id



-- Main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
