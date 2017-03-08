module Screen exposing (..)

import Window
import Config


scaleConstant : Float
scaleConstant =
    sqrt (toFloat Config.screenSurfaceArea)


type alias Model =
    { actualWidth : Int
    , actualHeight : Int
    }


init : Model
init =
    { actualWidth = 0
    , actualHeight = 0
    }


resize { width, height } model =
    { model | actualWidth = width, actualHeight = height }


toActual : Model -> Float -> Float
toActual model value =
    (scale model) * value


width model =
    toFloat model.actualWidth / scale model


height model =
    toFloat model.actualHeight / scale model


scale : Model -> Float
scale model =
    sqrt
        (toFloat model.actualHeight * toFloat model.actualWidth)
        / scaleConstant


actualWidth model =
    model.actualWidth


actualHeight model =
    model.actualHeight
