module Game.Model exposing (..)


type Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT


type Action
    = BUILD
    | MOVE
    | NONE


type alias Entity =
    { action : Action
    , direction : Direction
    , x : Int
    , y : Int
    }


type alias Model =
    { hero : Entity
    , creeps : List Entity
    , blocks : List Entity
    }



--    { hero : Entity.Model
--    , creeps : List Entity.Model
--    , ticksUntilSpawn : Int
