module Game.Model exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


type Action
    = NoAction
    | Move
    | Build
    | Attack
    | KillHero


type EntityType
    = Hero
    | Creep
    | Block


type alias Entity =
    { action : Action
    , direction : Direction
    , x : Int
    , y : Int
    , px : Int
    , py : Int
    , kind : EntityType
    , health : Int
    }


type alias Model =
    { hero : Entity
    , creeps : List Entity
    , blocks : List Entity
    , gameOver : Bool
    }
