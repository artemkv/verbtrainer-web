module Presente exposing (..)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (..)


spanishPresente : String -> Html msg
spanishPresente exerciseListLink =
    div [ class "grammar-topic" ]
        [ div [ class "grammar-topic-name" ] [ text "Presente" ]
        , div [ class "grammar-topic-description" ]
            [ a
                [ class "grammar-to-exercises-link"
                , href exerciseListLink
                ]
                [ text "click_to_practice" ]
            ]
        , div [ class "grammar-topic-content" ] []
        ]
