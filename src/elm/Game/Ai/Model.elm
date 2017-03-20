module Game.Ai.Model exposing (..)

import Game.Model exposing (..)


type alias BuildPlan =
    { x : Int
    , y : Int
    , build : EntityType
    , eval : Float
    }
