port module Model exposing (Model)

import Game.Model as Game
import Screen


type alias Model =
    { timeUntilGameUpdate : Float
    , game : Game.Model
    , screen : Screen.Model
    }
