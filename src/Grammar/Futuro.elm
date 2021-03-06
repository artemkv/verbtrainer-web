module Grammar.Futuro exposing (..)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (..)
import I18Next exposing (Translations)
import Translations.Grammar
import Translations.SpanishGrammar

spanishFuturo : String -> Translations -> Html msg
spanishFuturo exerciseListLink translations =
    div [ class "grammar-topic" ]
        [ div [ class "grammar-topic-name" ]
            [ text "Futuro" ]
        , div [ class "grammar-topic-description" ]
            [ a
                [ class "grammar-to-exercises-link"
                , href exerciseListLink
                ]
                [ text <| Translations.Grammar.clickToPractice translations ]
            ]
        , div [ class "grammar-topic-content" ]
            [ div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text <| Translations.SpanishGrammar.regularVerbs translations ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text <| Translations.SpanishGrammar.xxVerbsPrefix translations
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-ar" ]
                            , text <| Translations.SpanishGrammar.xxVerbsSuffix translations
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Hablar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "é" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "ás" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "á" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hablar"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "án" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text <| Translations.SpanishGrammar.xxVerbsPrefix translations
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-er" ]
                            , text <| Translations.SpanishGrammar.xxVerbsSuffix translations
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Comer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "é" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "ás" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "á" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "comer"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "án" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text <| Translations.SpanishGrammar.xxVerbsPrefix translations
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-ir" ]
                            , text <| Translations.SpanishGrammar.xxVerbsSuffix translations
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Escribir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "é" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "ás" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "á" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escribir"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "án" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text <| Translations.SpanishGrammar.irregularVerbsDecirHacerPoder translations ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Decir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "diré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "dirás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "dirá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "diremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "diréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "dirán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Hacer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "haré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "harás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hará" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "haremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "haréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "harán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Poder" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "podré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "podrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "podrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "podremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "podréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "podrán" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text <| Translations.SpanishGrammar.irregularVerbsPonerQuererSaber translations ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Poner" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "pondré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pondrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pondrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pondremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pondréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pondrán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Querer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "querré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "querrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "querrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "querremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "querréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "querrán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Saber" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "sabré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sabrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sabrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sabremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sabréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sabrán" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text <| Translations.SpanishGrammar.irregularVerbsSalirTenerVenir translations ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Salir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "saldré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "saldrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "saldrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "saldremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "saldréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "saldrán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Tener" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "tendré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "tendrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "tendrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "tendremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "tendréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "tendrán" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Venir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "vendré" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vendrás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vendrá" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vendremos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vendréis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vendrán" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
