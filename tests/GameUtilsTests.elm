module GameUtilsTests exposing (..)

import Expect
import Test exposing (..)
import Game.Utils
import Helpers exposing (..)


gameUtilsTests : Test
gameUtilsTests =
    describe "Game utils"
        [ describe "distance"
            [ test "gets the distance between two entities" <|
                \() ->
                    let
                        e1 =
                            createBlock -1 1

                        e2 =
                            createBlock 2 5
                    in
                        Game.Utils.distance e1 e2
                            |> Expect.equal 7
            ]
        , describe "distanceFromCenter"
            [ test "gets the distance from the center" <|
                \() ->
                    Game.Utils.distanceFromCenter (createBlock 2 5)
                        |> Expect.equal 7
            ]
        ]
