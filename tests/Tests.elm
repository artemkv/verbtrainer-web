module Tests exposing (..)

import Expect exposing (..)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Business Logic"
        [ test "joins answers together" <|
            \() ->
                [ "aaa", "bbb", "ccc" ] |> joined |> Expect.equal "aaa/bbb/ccc"
        , test "answer is checked case-insensitive" <|
            \() ->
                isEqualCI "hello" "Hello" |> Expect.true "Expected answer to match case-insensitive"
        , test "answer is checked accent-sensitive" <|
            \() ->
                isEqualCI "hello" "héllo" |> Expect.false "Expected answer to match accent-sensitive"
        , test "answer starts with itself" <|
            \() ->
                startsWithCI "hello" "hello" |> Expect.true "Expected answer to start with itself"
        , test "answer starts with empty string" <|
            \() ->
                startsWithCI "" "hello" |> Expect.true "Expected answer to start with empty string"
        , test "answer starts with is checked case-insensitive" <|
            \() ->
                startsWithCI "hell" "Hello" |> Expect.true "Expected check starts with as case-insensitive"
        , test "answer starts with is checked accent-sensitive" <|
            \() ->
                startsWithCI "hell" "héllo" |> Expect.false "Expected check starts with as accent-sensitive"
        , test "value is completed when matches any of the answers" <|
            \() ->
                isCompleted [ "hello", "world" ] "world" |> Expect.true "Expected to be considered completed"
        , test "value is not completed when does not match any of the answers" <|
            \() ->
                isCompleted [ "hello", "world" ] "moon" |> Expect.false "Expected to be not considered completed"

        -- TODO: write more tests
        ]
