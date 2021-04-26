module Presente exposing (..)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (..)


spanishPresente : String -> Html msg
spanishPresente exerciseListLink =
    div [ class "grammar-topic" ]
        [ div [ class "grammar-topic-name" ]
            [ text "Presente" ]
        , div [ class "grammar-topic-description" ]
            [ a
                [ class "grammar-to-exercises-link"
                , href exerciseListLink
                ]
                [ text "click_to_practice" ]
            ]
        , div [ class "grammar-topic-content" ]
            [ div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text (localize "spanish_grammar.regular_verbs") ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text (localize "spanish_grammar.xx_verbs_prefix")
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-ar" ]
                            , text (localize "spanish_grammar.xx_verbs_suffix")
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Hablar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "as" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "a" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "amos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "áis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "habl"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "an" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text (localize "spanish_grammar.xx_verbs_prefix")
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-er" ]
                            , text (localize "spanish_grammar.xx_verbs_suffix")
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Comer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "com"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text (localize "spanish_grammar.xx_verbs_prefix")
                            , span [ class "grammar-topic-verb-form-ending" ]
                                [ text "-ir" ]
                            , text (localize "spanish_grammar.xx_verbs_suffix")
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Escribir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "imos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "ís" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "escrib"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text (localize "spanish_grammar.radical_changing_verbs") ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text "o -> "
                            , span [ class "grammar-topic-irregular" ]
                                [ text "ue" ]
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Poder" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "ue" ]
                                    , text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "ue" ]
                                    , text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "ue" ]
                                    , text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pod"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pod"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "ue" ]
                                    , text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text "u -> "
                            , span [ class "grammar-topic-irregular" ]
                                [ text "ue" ]
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Jugar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "ju"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "e" ]
                                    , text "g"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "ju"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "e" ]
                                    , text "g"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "as" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "ju"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "e" ]
                                    , text "g"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "a" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "jug"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "amos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "jug"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "áis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "ju"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "e" ]
                                    , text "g"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "an" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text "e -> "
                            , span [ class "grammar-topic-irregular" ]
                                [ text "ie" ]
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Pensar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "ens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "ens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "as" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "ens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "a" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "amos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "áis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "p"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "ens"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "an" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-subsection-title" ]
                            [ text "e -> "
                            , span [ class "grammar-topic-irregular" ]
                                [ text "i" ]
                            ]
                        , div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Servir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "s"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "rv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "s"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "rv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "s"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "rv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "serv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "imos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "serv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "ís" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "s"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "rv"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text (localize "spanish_grammar.irregular_verbs_ir_ser_estar") ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Ir" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "voy" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vas" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "va" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vamos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "vais" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "van" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Ser" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "soy" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "eres" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "es" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "somos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sois" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "son" ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Estar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "estoy" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "estás" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "está" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "estamos" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "estáis" ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "están" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text (localize "spanish_grammar.irregular_verbs_dar_tener_poner") ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Dar" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "oy" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "as" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "a" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "amos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "ais" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "d"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "an" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Tener" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "ten"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "g" ]
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "t"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "en"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "t"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "en"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "ten"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "ten"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "t"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "i" ]
                                    , text "en"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Poner" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "g" ]
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "pon"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "grammar-topic-section" ]
                [ div [ class "grammar-topic-section-title" ]
                    [ text (localize "spanish_grammar.irregular_verbs_hacer_saber_conocer") ]
                , div [ class "grammar-topic-section-content" ]
                    [ div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Hacer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "ha"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "g" ]
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hac"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hac"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hac"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hac"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "hac"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Saber" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ span [ class "grammar-topic-irregular" ]
                                        [ text "sé" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sab"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sab"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sab"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sab"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "sab"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "grammar-topic-subsection" ]
                        [ div [ class "grammar-topic-verb-conjugation" ]
                            [ div [ class "grammar-topic-verb" ]
                                [ text "Conocer" ]
                            , div [ class "grammar-topic-verb-forms" ]
                                [ div [ class "grammar-topic-verb-form" ]
                                    [ text "cono"
                                    , span [ class "grammar-topic-irregular" ]
                                        [ text "z" ]
                                    , text "c"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "o" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "conoc"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "es" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "conoc"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "e" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "conoc"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "emos" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "conoc"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "éis" ]
                                    ]
                                , div [ class "grammar-topic-verb-form" ]
                                    [ text "conoc"
                                    , span [ class "grammar-topic-verb-form-ending" ]
                                        [ text "en" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


localize : String -> String
localize text =
    -- TODO: this is a dummy implementation
    case text of
        "spanish_grammar.regular_verbs" ->
            "Regular verbs"

        "spanish_grammar.xx_verbs_prefix" ->
            ""

        "spanish_grammar.xx_verbs_suffix" ->
            " verbs"

        "spanish_grammar.radical_changing_verbs" ->
            "Radical changing verbs"

        "spanish_grammar.irregular_verbs_ir_ser_estar" ->
            "Irregular verbs - Ir, Ser, Estar"

        "spanish_grammar.irregular_verbs_dar_tener_poner" ->
            "Irregular verbs - Dar, Tener, Poner"

        "spanish_grammar.irregular_verbs_hacer_saber_conocer" ->
            "Irregular verbs - Hacer, Saber, Conocer"

        _ ->
            "__" ++ text ++ "__"
