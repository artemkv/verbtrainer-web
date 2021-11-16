port module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, img, input, label, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import I18Next
import Json.Decode exposing (Decoder, bool, decodeString, field, int, map2, map3, map4, map6, map8, string)
import Json.Encode
import Presente exposing (spanishPresente)
import String exposing (fromInt)
import Task
import Translations.Exercises
import Url
import Url.Parser exposing ((</>), Parser)
import Util exposing (conditionallyPick)



-- Model


type Model
    = Initialized ExtendedModel
    | FailedToInitialize String


type alias ExtendedModel =
    { navKey : Nav.Key
    , translations : I18Next.Translations
    , appModel : AppModel
    }


type AppModel
    = ExerciseListNotLoaded
    | ExerciseListLoadingFailed String
    | ExerciseListInProgress ExerciseListData ExerciseListProgress
    | ExerciseNotLoaded
    | ExerciseLoadingFailed String
    | ExerciseInProgress ExerciseSpec ExerciseCurrentState ExerciseListProgress
    | ExerciseCompleted ExerciseSpec ExerciseSummary ExerciseListProgress
    | GrammarTopicContent ExerciseListId
    | Error String
    | PageNotFound


type alias ExerciseId =
    String


type alias ExerciseListId =
    String


type alias ExerciseListData =
    { id : ExerciseListId
    , title : String
    , subtitle : String
    , exercises : List ExerciseDescription
    }


type alias ExerciseDescription =
    { id : ExerciseId
    , name : String
    }


type alias ExerciseSpec =
    { id : ExerciseId
    , listId : ExerciseListId
    , verb : String
    , tense : String
    , labels : ExerciseLabels
    , answers : ExerciseAnswers
    , exercisesInList : Int
    , next : NextExerciseData
    }


type alias ExerciseLabels =
    { firstSingular : String
    , secondSingular : String
    , thirdSingular : String
    , firstPlural : String
    , secondPlural : String
    , thirdPlural : String
    }


type alias ExerciseAnswers =
    { firstSingular : List String
    , secondSingular : List String
    , thirdSingular : List String
    , firstPlural : List String
    , secondPlural : List String
    , thirdPlural : List String
    }


type alias NextExerciseData =
    { id : ExerciseId
    , verb : String
    }


type alias ExerciseCurrentState =
    { firstSingular : FillBoxState
    , secondSingular : FillBoxState
    , thirdSingular : FillBoxState
    , firstPlural : FillBoxState
    , secondPlural : FillBoxState
    , thirdPlural : FillBoxState
    , activeFillBox : FillBoxReference
    }


type FillBoxReference
    = FillBox VerbForm


type VerbForm
    = FirstSingular
    | SecondSingular
    | ThirdSingular
    | FirstPlural
    | SecondPlural
    | ThirdPlural


type alias FillBoxState =
    { value : String
    , isCompleted : Bool
    , errorCount : Int
    }


type ExerciseListProgress
    = NotSynchronized
    | Synchronized ExerciseListProgressData
    | SynchronizationFailed String


type alias ExerciseListProgressData =
    { id : ExerciseListId
    , exercises : List ExerciseProgressData
    }


type alias ExerciseProgressData =
    { id : ExerciseId
    , isPerfect : Bool
    }


type alias ExerciseListProgressStats =
    { completedAndPerfect : Int
    , completedNotPerfect : Int
    , notCompleted : Int
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
    , thirdSingular = emptyFillBoxState
    , firstPlural = emptyFillBoxState
    , secondPlural = emptyFillBoxState
    , thirdPlural = emptyFillBoxState
    , activeFillBox = FillBox FirstSingular
    }


type alias ExerciseSummary =
    { overallResult : String
    , feedback : String
    , finalResults : ExerciseFinalResults
    }


type alias ExerciseFinalResults =
    { firstSingular : FinalResult
    , secondSingular : FinalResult
    , thirdSingular : FinalResult
    , firstPlural : FinalResult
    , secondPlural : FinalResult
    , thirdPlural : FinalResult
    }


type FinalResult
    = Correct
    | Incorrect


init : Json.Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    case Json.Decode.decodeValue I18Next.translationsDecoder flags of
        Ok translations ->
            handleRouteChange url (ExtendedModel navKey translations ExerciseListNotLoaded)
                |> toInitialized

        Err err ->
            ( FailedToInitialize (Json.Decode.errorToString err), Cmd.none )


toInitialized : ( ExtendedModel, Cmd Msg ) -> ( Model, Cmd Msg )
toInitialized ( extendedmodel, cmd ) =
    ( Initialized extendedmodel, cmd )



-- Update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MoveToExercise ExerciseId
    | ExerciseDataReceived String
    | ExerciseListDataReceived String
    | ExerciseListProgressDataReceived String
    | FirstSingularChange String
    | SecondSingularChange String
    | ThirdSingularChange String
    | FirstPluralChange String
    | SecondPluralChange String
    | ThirdPluralChange String
    | FirstSingularFocused
    | SecondSingularFocused
    | ThirdSingularFocused
    | FirstPluralFocused
    | SecondPluralFocused
    | ThirdPluralFocused
    | VirtualKeyPressed String
    | RetryCompletedExercise
    | FocusResult (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Initialized extendedModel ->
            updateExtendedModel msg extendedModel |> toInitialized

        FailedToInitialize err ->
            ( FailedToInitialize err, Cmd.none )


updateExtendedModel : Msg -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
updateExtendedModel msg model =
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

        ExerciseListDataReceived data ->
            updateExerciseListFromReceivedData data appModel |> asNewAppModelPlusCommandOf model

        ExerciseListProgressDataReceived data ->
            updateExerciseListProgressFromReceivedData data appModel |> asNewAppModelOf model |> justModel

        ExerciseDataReceived data ->
            updateExerciseFromReceivedData data appModel |> asNewAppModelPlusCommandOf model

        FirstSingularChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        SecondSingularChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        ThirdSingularChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        FirstPluralChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        SecondPluralChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        ThirdPluralChange _ ->
            updateExerciseInProgress msg appModel |> asNewAppModelPlusCommandOf model

        FirstSingularFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        SecondSingularFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        ThirdSingularFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        FirstPluralFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        SecondPluralFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        ThirdPluralFocused ->
            handleFillBoxFocused msg appModel |> asNewAppModelOf model |> justModel

        VirtualKeyPressed char ->
            handleVirtualKeyPress char appModel |> asNewAppModelPlusCommandOf model

        RetryCompletedExercise ->
            ( clearExerciseState appModel |> asNewAppModelOf model, focusFillBox (FillBox FirstSingular) )

        FocusResult result ->
            handleFocusResult model result


asNewAppModelOf : ExtendedModel -> AppModel -> ExtendedModel
asNewAppModelOf model appModel =
    { model | appModel = appModel }


asNewAppModelPlusCommandOf : ExtendedModel -> ( AppModel, Cmd Msg ) -> ( ExtendedModel, Cmd Msg )
asNewAppModelPlusCommandOf model ( appModel, cmd ) =
    ( appModel |> asNewAppModelOf model, cmd )


justModel : ExtendedModel -> ( ExtendedModel, Cmd Msg )
justModel model =
    ( model, Cmd.none )



-- Navigation


navigate : Browser.UrlRequest -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
navigate urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


handleRouteChange : Url.Url -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
handleRouteChange url model =
    case toRoute url of
        -- TODO: handle home correctly
        Home ->
            ( ExerciseListNotLoaded |> asNewAppModelOf model, requestExerciseListData "presente" )

        ExerciseList id ->
            ( ExerciseListNotLoaded |> asNewAppModelOf model, requestExerciseListData id )

        Exercise id ->
            ( ExerciseNotLoaded |> asNewAppModelOf model, requestExerciseData id )

        GrammarTopic id ->
            GrammarTopicContent id |> asNewAppModelOf model |> justModel

        NotFound ->
            PageNotFound |> asNewAppModelOf model |> justModel


navigateToExercise : ExerciseId -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
navigateToExercise id model =
    ( model, Nav.pushUrl model.navKey (getExerciseLink id) )



-- Focus management


focusElement : String -> Cmd Msg
focusElement elementId =
    Dom.focus elementId |> Task.attempt FocusResult


handleFocusResult : ExtendedModel -> Result Dom.Error () -> ( ExtendedModel, Cmd Msg )
handleFocusResult model result =
    case result of
        Err (Dom.NotFound id) ->
            -- TODO: should this error be on the model level to show it simply as warning instead of breaking the complete flow?
            ("Could not find DOM element with id " ++ id)
                |> Error
                |> asNewAppModelOf model
                |> justModel

        Ok () ->
            model |> justModel


focusFillBox : FillBoxReference -> Cmd Msg
focusFillBox reference =
    let
        (FillBox verbForm) =
            reference
    in
    getFillBoxElementId verbForm |> focusElement


handleFillBoxFocused : Msg -> AppModel -> AppModel
handleFillBoxFocused msg model =
    case model of
        ExerciseInProgress spec state progress ->
            case msg of
                FirstSingularFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox FirstSingular } progress

                SecondSingularFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox SecondSingular } progress

                ThirdSingularFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox ThirdSingular } progress

                FirstPluralFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox FirstPlural } progress

                SecondPluralFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox SecondPlural } progress

                ThirdPluralFocused ->
                    ExerciseInProgress spec { state | activeFillBox = FillBox ThirdPlural } progress

                _ ->
                    model

        _ ->
            model



-- Other model updates


updateExerciseListFromReceivedData : String -> AppModel -> ( AppModel, Cmd Msg )
updateExerciseListFromReceivedData data model =
    case model of
        ExerciseListNotLoaded ->
            decodeExerciseListDataOrError data |> onExerciseListLoad

        _ ->
            ( model, Cmd.none )


onExerciseListLoad : AppModel -> ( AppModel, Cmd Msg )
onExerciseListLoad model =
    case model of
        ExerciseListInProgress data _ ->
            ( model, requestExerciseListProgressData data.id )

        _ ->
            ( model, Cmd.none )


updateExerciseListProgressFromReceivedData : String -> AppModel -> AppModel
updateExerciseListProgressFromReceivedData data model =
    case model of
        ExerciseInProgress spec state _ ->
            ExerciseInProgress spec state (decodeExerciseListProgressDataOrError data)

        ExerciseCompleted spec summary _ ->
            ExerciseCompleted spec summary (decodeExerciseListProgressDataOrError data)

        ExerciseListInProgress listData _ ->
            ExerciseListInProgress listData (decodeExerciseListProgressDataOrError data)

        _ ->
            model


updateExerciseFromReceivedData : String -> AppModel -> ( AppModel, Cmd Msg )
updateExerciseFromReceivedData data model =
    case model of
        ExerciseNotLoaded ->
            decodeExerciseDataOrError data |> onExerciseStart

        _ ->
            ( model, Cmd.none )


onExerciseStart : AppModel -> ( AppModel, Cmd Msg )
onExerciseStart model =
    case model of
        ExerciseInProgress spec _ _ ->
            ( model
            , Cmd.batch
                [ requestExerciseListProgressData spec.listId
                , focusFillBox (FillBox FirstSingular)
                ]
            )

        _ ->
            ( model, Cmd.none )


updateExerciseInProgress : Msg -> AppModel -> ( AppModel, Cmd Msg )
updateExerciseInProgress msg model =
    case model of
        ExerciseInProgress spec state progress ->
            case msg of
                FirstSingularChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | firstSingular = getNewFillBoxState spec.answers.firstSingular newValue state.firstSingular }
                        progress

                SecondSingularChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | secondSingular = getNewFillBoxState spec.answers.secondSingular newValue state.secondSingular }
                        progress

                ThirdSingularChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | thirdSingular = getNewFillBoxState spec.answers.thirdSingular newValue state.thirdSingular }
                        progress

                FirstPluralChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | firstPlural = getNewFillBoxState spec.answers.firstPlural newValue state.firstPlural }
                        progress

                SecondPluralChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | secondPlural = getNewFillBoxState spec.answers.secondPlural newValue state.secondPlural }
                        progress

                ThirdPluralChange newValue ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | thirdPlural = getNewFillBoxState spec.answers.thirdPlural newValue state.thirdPlural }
                        progress

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


getNewFillBoxState : List String -> String -> FillBoxState -> FillBoxState
getNewFillBoxState answers newValue state =
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
    FillBoxState newValue newIsCompleted newErrorCount


handleVirtualKeyPress : String -> AppModel -> ( AppModel, Cmd Msg )
handleVirtualKeyPress char model =
    case model of
        ExerciseInProgress spec state progress ->
            case state.activeFillBox of
                FillBox FirstSingular ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | firstSingular = getNewFillBoxState spec.answers.firstSingular (state.firstSingular.value ++ char) state.firstSingular }
                        progress

                FillBox SecondSingular ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | secondSingular = getNewFillBoxState spec.answers.secondSingular (state.secondSingular.value ++ char) state.secondSingular }
                        progress

                FillBox ThirdSingular ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | thirdSingular = getNewFillBoxState spec.answers.thirdSingular (state.thirdSingular.value ++ char) state.thirdSingular }
                        progress

                FillBox FirstPlural ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | firstPlural = getNewFillBoxState spec.answers.firstPlural (state.firstPlural.value ++ char) state.firstPlural }
                        progress

                FillBox SecondPlural ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | secondPlural = getNewFillBoxState spec.answers.secondPlural (state.secondPlural.value ++ char) state.secondPlural }
                        progress

                FillBox ThirdPlural ->
                    returnAsExerciseInProgressOrCompleted
                        spec
                        { state | thirdPlural = getNewFillBoxState spec.answers.thirdPlural (state.thirdPlural.value ++ char) state.thirdPlural }
                        progress

        _ ->
            ( model, Cmd.none )


returnAsExerciseInProgressOrCompleted : ExerciseSpec -> ExerciseCurrentState -> ExerciseListProgress -> ( AppModel, Cmd Msg )
returnAsExerciseInProgressOrCompleted spec state progress =
    if isExerciseCompleted state then
        let
            incorrectTotal =
                (isPerfect state.firstSingular.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.secondSingular.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.thirdSingular.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.firstPlural.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.secondPlural.errorCount |> conditionallyPick 0 1)
                    + (isPerfect state.thirdPlural.errorCount |> conditionallyPick 0 1)

            isExercisePerfect =
                incorrectTotal == 0

            ( overallResult, feedback ) =
                getExerciseOverallResultAndFeedback incorrectTotal
        in
        ( ExerciseCompleted spec
            { overallResult = overallResult
            , feedback = feedback
            , finalResults =
                { firstSingular = finalResult state.firstSingular.errorCount
                , secondSingular = finalResult state.secondSingular.errorCount
                , thirdSingular = finalResult state.thirdSingular.errorCount
                , firstPlural = finalResult state.firstPlural.errorCount
                , secondPlural = finalResult state.secondPlural.errorCount
                , thirdPlural = finalResult state.thirdPlural.errorCount
                }
            }
            progress
        , Cmd.batch
            [ sendExerciseProgressData ( spec.id, isExercisePerfect )
            , focusElement exerciseCompletionScoreNextButtonId
            ]
        )

    else
        ( ExerciseInProgress spec state progress, focusFillBox state.activeFillBox )


clearExerciseState : AppModel -> AppModel
clearExerciseState model =
    case model of
        ExerciseCompleted spec _ progress ->
            ExerciseInProgress spec emptyExerciseState progress

        _ ->
            model



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ exerciseDataReceived ExerciseDataReceived
        , exerciseListDataReceived ExerciseListDataReceived
        , exerciseListProgressDataReceived ExerciseListProgressDataReceived
        ]



-- View


view : Model -> Browser.Document Msg
view model =
    let
        bodyContent =
            case model of
                Initialized extendedModel ->
                    [ body extendedModel.appModel extendedModel.translations ]

                FailedToInitialize err ->
                    [ div []
                        [ text <|
                            "Failed to initialize the app: '"
                                ++ err
                                ++ "'. Check that translations are loaded correctly."
                        ]
                    ]
    in
    -- TODO: where should the title come from?
    { title = "Verb Conjugation Exercises"
    , body = bodyContent
    }


body : AppModel -> I18Next.Translations -> Html Msg
body model translations =
    div []
        [ header
        , div
            [ class "content" ]
            [ content model translations ]
        ]


header : Html msg
header =
    div []
        [ div [ class "standard-header" ]
            [ div [ class "header" ]
                []
            ]
        ]


content : AppModel -> I18Next.Translations -> Html Msg
content model translations =
    case model of
        ExerciseListNotLoaded ->
            div [] [ spinner ]

        ExerciseListLoadingFailed reason ->
            -- TODO: render error correctly
            div [] [ text reason ]

        ExerciseListInProgress data progress ->
            exerciseList data progress

        ExerciseNotLoaded ->
            div [] [ spinner ]

        ExerciseLoadingFailed reason ->
            -- TODO: render error correctly
            div [] [ text reason ]

        ExerciseInProgress spec state progress ->
            verbConjugator spec state progress translations

        ExerciseCompleted spec summary progress ->
            verbConjugatorCompletionScore spec summary progress translations

        GrammarTopicContent id ->
            spanishPresente (getExerciseListLink id)

        Error err ->
            div [] [ text err ]

        PageNotFound ->
            notFound404


exerciseList : ExerciseListData -> ExerciseListProgress -> Html Msg
exerciseList data progress =
    div []
        [ controlBar [ nothing ]
        , div [ class "exercise-list" ]
            [ div [ class "exercise-list-title" ] [ text data.title ]
            , div [ class "exercise-list-subtitle" ]
                [ a
                    [ class "exercises-to-grammar-link"
                    , href (getGrammarTopicLink data.id)
                    ]
                    [ text data.subtitle ]
                ]
            , div [ class "exercise-list-exercises" ]
                (data.exercises |> List.map (exerciseLink progress))
            ]
        ]


exerciseLink : ExerciseListProgress -> ExerciseDescription -> Html Msg
exerciseLink progress description =
    div [ class "exercise-list-exercise-allow-access" ]
        [ a
            [ class "exercise-list-exercise-link"
            , href (getExerciseLink description.id)
            ]
            [ text description.name ]
        , exerciseLinkCompletion progress description.id
        ]


exerciseLinkCompletion : ExerciseListProgress -> ExerciseId -> Html Msg
exerciseLinkCompletion progress id =
    case progress of
        NotSynchronized ->
            nothing

        Synchronized progressData ->
            case getExerciseProgress progressData id of
                Completed isReferredExercisePerfect ->
                    isReferredExercisePerfect |> conditionallyPick completedAndPerfect completedNotPerfect

                NotCompleted ->
                    nothing

        -- TODO: Show warning somewhere
        SynchronizationFailed _ ->
            nothing


verbConjugator : ExerciseSpec -> ExerciseCurrentState -> ExerciseListProgress -> I18Next.Translations -> Html Msg
verbConjugator spec state progress translations =
    div []
        [ controlBar
            [ div [ class "control-bar-inner1" ]
                []
            , div [ class "control-bar-inner2" ]
                [ a
                    [ class "to-all-verbs-link"
                    , href (getExerciseListLink spec.listId)
                    ]
                    [ text <| Translations.Exercises.allVerbsLinks translations ]
                , exerciseProgress progress spec.exercisesInList
                ]
            , div [ class "control-bar-inner3" ]
                []
            ]
        , div [ class "verb-conjugator" ]
            [ div [ class "verb-conjugator-verb" ] [ text spec.verb ]
            , div [ class "verb-conjugator-tense" ]
                [ a
                    [ class "exercises-to-grammar-link"
                    , href (getGrammarTopicLink spec.listId)
                    ]
                    [ text spec.tense
                    ]
                ]
            , fillBox
                (getFillBoxElementId FirstSingular)
                spec.labels.firstSingular
                spec.answers.firstSingular
                state.firstSingular
                FirstSingularChange
                FirstSingularFocused
            , fillBox
                (getFillBoxElementId SecondSingular)
                spec.labels.secondSingular
                spec.answers.secondSingular
                state.secondSingular
                SecondSingularChange
                SecondSingularFocused
            , fillBox
                (getFillBoxElementId ThirdSingular)
                spec.labels.thirdSingular
                spec.answers.thirdSingular
                state.thirdSingular
                ThirdSingularChange
                ThirdSingularFocused
            , fillBox
                (getFillBoxElementId FirstPlural)
                spec.labels.firstPlural
                spec.answers.firstPlural
                state.firstPlural
                FirstPluralChange
                FirstPluralFocused
            , fillBox
                (getFillBoxElementId SecondPlural)
                spec.labels.secondPlural
                spec.answers.secondPlural
                state.secondPlural
                SecondPluralChange
                SecondPluralFocused
            , fillBox
                (getFillBoxElementId ThirdPlural)
                spec.labels.thirdPlural
                spec.answers.thirdPlural
                state.thirdPlural
                ThirdPluralChange
                ThirdPluralFocused
            ]
        , nextExerciseReference spec.next
        , virtualKeyboard
        ]


controlBar : List (Html Msg) -> Html Msg
controlBar inner =
    div [ class "control-bar" ]
        inner


exerciseProgress : ExerciseListProgress -> Int -> Html Msg
exerciseProgress progress exerciseTotalCount =
    case progress of
        NotSynchronized ->
            exerciseUnknownProgress

        SynchronizationFailed _ ->
            -- TODO: show warning somewhere
            exerciseUnknownProgress

        Synchronized progressData ->
            let
                stats =
                    getExerciseListProgressStats progressData exerciseTotalCount
            in
            div [ class "exercise-progress" ]
                [ completedAndPerfect
                , span [ class "exercise-progress-score" ] [ text (fromInt stats.completedAndPerfect) ]
                , completedNotPerfect
                , span [ class "exercise-progress-score" ] [ text (fromInt stats.completedNotPerfect) ]
                , notCompleted
                , span [ class "exercise-progress-score" ] [ text (fromInt stats.notCompleted) ]
                ]


exerciseUnknownProgress : Html Msg
exerciseUnknownProgress =
    div [ class "exercise-progress" ]
        [ completedAndPerfect
        , completedNotPerfect
        , notCompleted
        ]


getFillBoxElementId : VerbForm -> String
getFillBoxElementId verbForm =
    case verbForm of
        FirstSingular ->
            "firstSingular"

        SecondSingular ->
            "secondSingular"

        ThirdSingular ->
            "thirdSingular"

        FirstPlural ->
            "firstPlural"

        SecondPlural ->
            "secondPlural"

        ThirdPlural ->
            "thirdPlural"


type alias OnInputChangeMessageProducer =
    String -> Msg


fillBox : String -> String -> List String -> FillBoxState -> OnInputChangeMessageProducer -> Msg -> Html Msg
fillBox elementId labelText answers state onInputMsgProducer onFocusMsg =
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
                [ id elementId
                , class inputClass
                , type_ "text"
                , attribute "spellCheck" "false"
                , value state.value
                , onInput onInputMsgProducer
                , onFocus onFocusMsg
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


notCompleted : Html Msg
notCompleted =
    span [ class "not-completed-mark" ]
        [ img
            [ class "not-completed-image"
            , src "/notcompleted.png"
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


nextExerciseReference : NextExerciseData -> Html Msg
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


verbConjugatorCompletionScore : ExerciseSpec -> ExerciseSummary -> ExerciseListProgress -> I18Next.Translations -> Html Msg
verbConjugatorCompletionScore spec summary progress translations =
    div []
        [ controlBar
            [ div [ class "control-bar-inner1" ]
                []
            , div [ class "control-bar-inner2" ]
                [ a
                    [ class "to-all-verbs-link"
                    , href (getExerciseListLink spec.listId)
                    ]
                    [ text <| Translations.Exercises.allVerbsLinks translations ]
                , exerciseProgress progress spec.exercisesInList
                ]
            , div [ class "control-bar-inner3" ]
                []
            ]
        , div [ class "exercise-completion-score" ]
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
                    , resultBox
                        spec.labels.thirdSingular
                        spec.answers.thirdSingular
                        summary.finalResults.thirdSingular
                    , resultBox
                        spec.labels.firstPlural
                        spec.answers.firstPlural
                        summary.finalResults.firstPlural
                    , resultBox
                        spec.labels.secondPlural
                        spec.answers.secondPlural
                        summary.finalResults.secondPlural
                    , resultBox
                        spec.labels.thirdPlural
                        spec.answers.thirdPlural
                        summary.finalResults.thirdPlural
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
                        [ id exerciseCompletionScoreNextButtonId
                        , class "exercise-completion-score-next-button"
                        , onClick (MoveToExercise spec.next.id)
                        ]
                        -- TODO: move to labels
                        [ text (spec.next.verb ++ " >") ]
                    ]
                ]
            , div [ class "exercise-completion-score-next-inner3" ] []
            ]
        ]


exerciseCompletionScoreNextButtonId : String
exerciseCompletionScoreNextButtonId =
    "exercise-completion-score-next-button"


resultBox : String -> List String -> FinalResult -> Html Msg
resultBox labelText answers result =
    div []
        [ span [ class "result-box-correct-form" ]
            [ text (labelText ++ " " ++ (answers |> joined)) ]
        , span
            [ class "result-box-completion" ]
            [ result == Correct |> conditionallyPick completedAndPerfect completedNotPerfect ]
        ]


virtualKeyboard : Html Msg
virtualKeyboard =
    -- TODO: should be different letters for different languages
    div [ class "virtual-keyboard" ]
        [ virtualKey "á"
        , virtualKey "é"
        , virtualKey "í"
        , virtualKey "ó"
        , virtualKey "ú"
        , virtualKey "ü"
        , virtualKey "ñ"
        ]


virtualKey : String -> Html Msg
virtualKey char =
    div
        [ class "virtual-key"
        , onClick (VirtualKeyPressed char)
        ]
        [ span [ class "virtual-key-char" ] [ text char ]
        ]


spinner : Html Msg
spinner =
    div [ class "spinner" ]
        [ div [ class "spinner-container container1" ]
            [ div [ class "circle1" ] []
            , div [ class "circle2" ] []
            , div [ class "circle3" ] []
            , div [ class "circle4" ] []
            ]
        , div [ class "spinner-container container2" ]
            [ div [ class "circle1" ] []
            , div [ class "circle2" ] []
            , div [ class "circle3" ] []
            , div [ class "circle4" ] []
            ]
        , div [ class "spinner-container container3" ]
            [ div [ class "circle1" ] []
            , div [ class "circle2" ] []
            , div [ class "circle3" ] []
            , div [ class "circle4" ] []
            ]
        ]


notFound404 : Html Msg
notFound404 =
    div [] [ text "404" ]


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
    -- TODO: maybe should be renamed to isCorrect
    List.any (isEqualCI actual) <| expected


isCorrectSoFar : List String -> String -> Bool
isCorrectSoFar expected actual =
    List.any (startsWithCI actual) <| expected


isPerfect : Int -> Bool
isPerfect errorCount =
    errorCount <= 1


isExerciseCompleted : ExerciseCurrentState -> Bool
isExerciseCompleted state =
    state.firstSingular.isCompleted
        && state.secondSingular.isCompleted
        && state.thirdSingular.isCompleted
        && state.firstPlural.isCompleted
        && state.secondPlural.isCompleted
        && state.thirdPlural.isCompleted


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


getExerciseListProgressStats : ExerciseListProgressData -> Int -> ExerciseListProgressStats
getExerciseListProgressStats progress exerciseTotalCount =
    let
        completedAndPerfectTotal =
            progress.exercises
                |> List.map (.isPerfect >> conditionallyPick 1 0)
                |> List.sum

        completedNotPerfectTotal =
            progress.exercises
                |> List.map (.isPerfect >> conditionallyPick 0 1)
                |> List.sum
    in
    { completedAndPerfect = completedAndPerfectTotal
    , completedNotPerfect = completedNotPerfectTotal
    , notCompleted = exerciseTotalCount - completedAndPerfectTotal - completedNotPerfectTotal
    }


type ExerciseProgress
    = NotCompleted
    | Completed Bool


getExerciseProgress : ExerciseListProgressData -> ExerciseId -> ExerciseProgress
getExerciseProgress progressData id =
    let
        item =
            progressData.exercises
                |> List.filter (\x -> x.id == id)
                |> List.head
    in
    case item of
        Nothing ->
            NotCompleted

        Just exerciseProgressData ->
            Completed exerciseProgressData.isPerfect



-- Data loading


port requestExerciseListData : String -> Cmd id


port exerciseListDataReceived : (String -> msg) -> Sub msg


port requestExerciseData : String -> Cmd id


port exerciseDataReceived : (String -> msg) -> Sub msg


port requestExerciseListProgressData : String -> Cmd id


port exerciseListProgressDataReceived : (String -> msg) -> Sub msg


port sendExerciseProgressData : ( String, Bool ) -> Cmd id


decodeExerciseListDataOrError : String -> AppModel
decodeExerciseListDataOrError data =
    let
        isOk =
            data |> decodeString (field "isOk" bool)
    in
    case isOk of
        Ok success ->
            if success then
                decodeExerciseListData data

            else
                decodeExerciseListLoadingErrorData data

        Err err ->
            ExerciseListLoadingFailed (Json.Decode.errorToString err)


decodeExerciseListData : String -> AppModel
decodeExerciseListData data =
    let
        result =
            data |> decodeString exerciseListDataDecoder
    in
    case result of
        Ok exerciseListData ->
            ExerciseListInProgress exerciseListData NotSynchronized

        Err err ->
            ExerciseListLoadingFailed (Json.Decode.errorToString err)


decodeExerciseListLoadingErrorData : String -> AppModel
decodeExerciseListLoadingErrorData data =
    let
        result =
            data |> decodeString dataLoadingErrorDecoder
    in
    case result of
        Ok err ->
            ExerciseListLoadingFailed err

        Err err ->
            ExerciseListLoadingFailed (Json.Decode.errorToString err)


decodeExerciseDataOrError : String -> AppModel
decodeExerciseDataOrError data =
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
            ExerciseInProgress spec emptyExerciseState NotSynchronized

        Err err ->
            ExerciseLoadingFailed (Json.Decode.errorToString err)


decodeExerciseLoadingErrorData : String -> AppModel
decodeExerciseLoadingErrorData data =
    let
        result =
            data |> decodeString dataLoadingErrorDecoder
    in
    case result of
        Ok err ->
            ExerciseLoadingFailed err

        Err err ->
            ExerciseLoadingFailed (Json.Decode.errorToString err)


exerciseListDataDecoder : Decoder ExerciseListData
exerciseListDataDecoder =
    field "data" <|
        map4 ExerciseListData
            (field "id" string)
            (field "title" string)
            (field "subtitle" string)
            (field "exercises" (Json.Decode.list exerciseListItemDataDecoder))


exerciseListItemDataDecoder : Decoder ExerciseDescription
exerciseListItemDataDecoder =
    map2 ExerciseDescription
        (field "id" string)
        (field "name" string)


decodeExerciseListProgressDataOrError : String -> ExerciseListProgress
decodeExerciseListProgressDataOrError data =
    let
        isOk =
            data |> decodeString (field "isOk" bool)
    in
    case isOk of
        Ok success ->
            if success then
                decodeExerciseListProgressData data

            else
                decodeExerciseListProgressLoadingErrorData data

        Err err ->
            SynchronizationFailed (Json.Decode.errorToString err)


decodeExerciseListProgressData : String -> ExerciseListProgress
decodeExerciseListProgressData data =
    let
        result =
            data |> decodeString exerciseListProgressDataDecoder
    in
    case result of
        Ok progress ->
            Synchronized progress

        Err err ->
            SynchronizationFailed (Json.Decode.errorToString err)


decodeExerciseListProgressLoadingErrorData : String -> ExerciseListProgress
decodeExerciseListProgressLoadingErrorData data =
    let
        result =
            data |> decodeString dataLoadingErrorDecoder
    in
    case result of
        Ok err ->
            SynchronizationFailed err

        Err err ->
            SynchronizationFailed (Json.Decode.errorToString err)


exerciseListProgressDataDecoder : Decoder ExerciseListProgressData
exerciseListProgressDataDecoder =
    field "data" <|
        map2 ExerciseListProgressData
            (field "id" string)
            (field "exercises" (Json.Decode.list exerciseProgressDataDecoder))


exerciseProgressDataDecoder : Decoder ExerciseProgressData
exerciseProgressDataDecoder =
    map2 ExerciseProgressData
        (field "id" string)
        (field "isPerfect" bool)


exerciseSpecDecoder : Decoder ExerciseSpec
exerciseSpecDecoder =
    field "data" <|
        map8 ExerciseSpec
            (field "id" string)
            (field "listId" string)
            (field "verb" string)
            (field "tense" string)
            (field "labels" exerciseLabelsDecoder)
            (field "answers" exerciseAnswersDecoder)
            (field "exercisesInList" int)
            (field "next" exerciseNextDecoder)


exerciseLabelsDecoder : Decoder ExerciseLabels
exerciseLabelsDecoder =
    map6 ExerciseLabels
        (field "firstSingular" string)
        (field "secondSingular" string)
        (field "thirdSingular" string)
        (field "firstPlural" string)
        (field "secondPlural" string)
        (field "thirdPlural" string)


exerciseAnswersDecoder : Decoder ExerciseAnswers
exerciseAnswersDecoder =
    map6 ExerciseAnswers
        (field "firstSingular" (Json.Decode.list string))
        (field "secondSingular" (Json.Decode.list string))
        (field "thirdSingular" (Json.Decode.list string))
        (field "firstPlural" (Json.Decode.list string))
        (field "secondPlural" (Json.Decode.list string))
        (field "thirdPlural" (Json.Decode.list string))


exerciseNextDecoder : Decoder NextExerciseData
exerciseNextDecoder =
    map2 NextExerciseData
        (field "id" string)
        (field "verb" string)


dataLoadingErrorDecoder : Decoder String
dataLoadingErrorDecoder =
    field "data" (field "err" string)



-- Routing


type Route
    = Home
    | ExerciseList ExerciseListId
    | Exercise ExerciseId
    | GrammarTopic ExerciseListId
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Exercise (Url.Parser.s "exercise" </> Url.Parser.string)
        , Url.Parser.map ExerciseList (Url.Parser.s "exercise-list" </> Url.Parser.string)
        , Url.Parser.map GrammarTopic (Url.Parser.s "grammar-topic" </> Url.Parser.string)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Url.Parser.parse routeParser url |> Maybe.withDefault NotFound


getExerciseListLink : ExerciseListId -> String
getExerciseListLink id =
    "/exercise-list/" ++ id


getExerciseLink : ExerciseId -> String
getExerciseLink id =
    "/exercise/" ++ id


getGrammarTopicLink : ExerciseListId -> String
getGrammarTopicLink id =
    "/grammar-topic/" ++ id



-- Main


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
