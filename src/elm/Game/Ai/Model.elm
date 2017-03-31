module Game.Ai.Model exposing (..)

import Game.Model exposing (..)


type alias BuildPlan =
    { x : Int
    , y : Int
    , build : StructureType
    , eval : Float
    --, d : Float
    --, t : Float
    --, c : Float
    --, h : Float
    }
