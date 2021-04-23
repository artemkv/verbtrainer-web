module Tests exposing (..)

import Expect exposing (equal)
import Test exposing (..)


suite : Test
suite =
    describe "Business Logic"
        [ test "two plus two equals four" <|
            \() -> equal 4 (2 + 2)
        ]
