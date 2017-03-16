module Tests exposing (..)

import Expect
import Fuzz exposing (list, int, tuple, string)
import Game.Block as Block
import Game.Creep as Creep
import Game.Hero as Hero
import Game.Model exposing (..)
import Game.State
import String
import Test exposing (..)
import Utils exposing (..)


initModel =
    Game.State.init


createHero x y action direction =
    let
        hero =
            Hero.create
    in
        { hero | x = x, y = y, action = action, direction = direction }


createCreep x y action direction =
    let
        creep =
            Creep.create
    in
        { creep | x = x, y = y, action = action, direction = direction }


createBlock : Int -> Int -> Entity
createBlock x y =
    let
        block =
            Block.create
    in
        { block | x = x, y = y }


all : Test
all =
    describe "Game"
        [ test "kills hero next to creep" <|
            \() ->
                let
                    {-
                       _ _ _
                       E H _
                       _ _ _
                    -}
                    model =
                        { initModel
                            | hero = createHero 0 0 NoAction Down
                            , creeps =
                                [ createCreep -1 0 NoAction Down ]
                        }
                in
                    Game.State.update model
                        |> .gameOver
                        >> Expect.equal True
        , test "kills trapped hero diagonal to creep" <|
            \() ->
                let
                    {-
                       _ B _
                       _ H B
                       E _ _
                    -}
                    model =
                        { initModel
                            | hero = createHero 0 0 NoAction Down
                            , creeps =
                                [ createCreep -1 -1 NoAction Down ]
                            , blocks =
                                [ createBlock 0 1
                                , createBlock 1 0
                                ]
                        }
                in
                    Game.State.update model
                        |> Expect.all
                            [ .gameOver >> Expect.equal False
                            , Game.State.update >> .gameOver >> Expect.equal True
                            ]
        , test "kills trapped hero by killing blocks" <|
            \() ->
                let
                    {-
                       _ B _
                       B H B
                       E B _
                    -}
                    model =
                        { initModel
                            | hero = createHero 0 0 NoAction Down
                            , creeps =
                                [ createCreep -1 -1 NoAction Down ]
                            , blocks =
                                [ createBlock 0 1
                                , createBlock 1 0
                                , createBlock -1 0
                                , createBlock 0 -1
                                ]
                        }
                in
                    Game.State.update model
                        |> Expect.all
                            -- creep is attacking the block
                            [ .gameOver >> Expect.equal False
                            , .creeps
                                >> List.all (\c -> c.action == Attack)
                                >> Expect.equal True
                              -- creep has killed the block
                            , Game.State.update
                                >> .gameOver
                                >> Expect.equal False
                            --  -- game is over
                            , Game.State.update
                                >> Game.State.update
                                >> .gameOver
                                >> Expect.equal True
                            ]
        ]
