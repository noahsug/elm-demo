module Game.Model exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


type Action
    = NoAction
    | Move Int
    | Build StructureType
    | Attack Entity
    | AttackArea
    | KillHero


type EntityType
    = Hero
    | Creep CreepType
    | Structure StructureType


type StructureType
    = Block
    | Turret
    | AoeTurret


type CreepType
    = Tank
    | Dmg


type alias Entity =
    { action : Action
    , direction : Direction
    , x : Int
    , y : Int
    , px : Int
    , py : Int
    , kind : EntityType
    , health : Int
    , dmg : Int
    , age : Int
    }


type alias Model =
    { hero : Entity
    , structures : List Entity
    , creeps : List Entity
    , creepLine : List Entity
    , creepsSpawned : Int
    , gameOver : Bool
    , ticks : Int
    }


type alias Movement =
    { x : Int
    , y : Int
    , direction : Direction
    , collide : Maybe Entity
    }
