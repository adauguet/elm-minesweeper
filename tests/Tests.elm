module Tests exposing (suite)

import Counter exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Counter"
        [ test "toDigitChar positive" <|
            \_ -> Expect.equal ( '0', '1', '0' ) (Counter.toDigitChar 10)
        , test "toDigitChar negative" <|
            \_ -> Expect.equal ( '-', '0', '1' ) (Counter.toDigitChar -1)
        , test "toDigitChar atLeast" <|
            \_ -> Expect.equal ( '-', '9', '9' ) (Counter.toDigitChar -100)
        , test "toDigitChar atMost" <|
            \_ -> Expect.equal ( '9', '9', '9' ) (Counter.toDigitChar 1000)
        ]
