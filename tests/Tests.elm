module Tests exposing (..)

import Expect
import GameTests exposing (gameTests)
import GameUtilsTests exposing (gameUtilsTests)
import Test exposing (..)


all : Test
all =
    Test.concat
        [ gameTests, gameUtilsTests ]
