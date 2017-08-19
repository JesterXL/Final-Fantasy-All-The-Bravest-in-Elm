module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import BattleUtils exposing (..)

suite : Test
suite =
    describe "BattleUtils"
        [ describe "divide"
            [ test "works with 10 divided by 2" <|
                \_ ->
                    let
                        result = divide 10 2
                    in
                        Expect.equal result (5)
            ]
        ]