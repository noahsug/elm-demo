port module Model exposing (..)

import Entity
import Screen


type alias Model =
    { hero : Entity.Model
    , creeps : List Entity.Model
    , timeUntilSpawn : Float
    , screen : Screen.Model
    }
