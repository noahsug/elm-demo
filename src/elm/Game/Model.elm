module Game.Model exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


type Action
    = NoAction
    | Move
    | Build StructureType
    | Attack Entity
    | KillHero


type EntityType
    = Hero
    | Creep
    | Structure StructureType


type StructureType
    = Block
    | Turret


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
    , structures : List Entity
    , gameOver : Bool
    }


type alias Movement =
    { x : Int
    , y : Int
    , direction : Direction
    , collide : Maybe Entity
    }
