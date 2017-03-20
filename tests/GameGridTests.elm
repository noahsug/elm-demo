module GameGridTests exposing (gameGridTests)

import Expect
import Test exposing (..)
import Game.Grid as Grid


gameGridTests : Test
gameGridTests =
    describe "Game grid"
        [ describe "positions"
            [ test "gets all legal hero positions" <|
                \() ->
                    List.length Grid.positions
                        |> Expect.equal 148
            ]
        ]
