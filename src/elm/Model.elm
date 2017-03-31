port module Model exposing (..)

import Game.Model as Game
import Screen


type State
    = Win
    | Lose
    | Playing


type alias Model =
    { timeUntilGameUpdate : Float
    , game : Game.Model
    , screen : Screen.Model
    , state : State
    , runUntil : Float
    }
