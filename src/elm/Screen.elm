module Screen exposing (..)

import Window


scaleConstant =
    sqrt (400 * 711)


type alias Model =
    { actualWidth : Int
    , actualHeight : Int
    }


init =
    { actualWidth = 0
    , actualHeight = 0
    }


resize { width, height } model =
    { model | actualWidth = width, actualHeight = height }


toActual model value =
    (scale model) * value


width model =
    toFloat model.actualWidth / scale model


height model =
    toFloat model.actualHeight / scale model


scale model =
    sqrt
        (toFloat model.actualHeight * toFloat model.actualWidth)
        / scaleConstant


actualWidth model =
    model.actualWidth


actualHeight model =
    model.actualHeight
