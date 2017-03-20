module Tests exposing (..)

import Expect
import GameTests exposing (gameTests)
import GameUtilsTests exposing (gameUtilsTests)
import GameGridTests exposing (gameGridTests)
import Test exposing (..)


all : Test
all =
    Test.concat
        [ gameTests, gameUtilsTests, gameGridTests ]
