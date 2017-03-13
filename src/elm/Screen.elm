module Screen exposing (..)

import Config
import Window


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


resize : Window.Size -> Model -> Model
resize { width, height } model =
    { model | actualWidth = width, actualHeight = height }


toActual : Model -> Float -> Float
toActual model value =
    (scale model) * value


width : Model -> Float
width model =
    toFloat model.actualWidth / scale model


height : Model -> Float
height model =
    toFloat model.actualHeight / scale model


scale : Model -> Float
scale model =
    sqrt
        (toFloat model.actualHeight * toFloat model.actualWidth)
        / scaleConstant


actualWidth : Model -> Int
actualWidth model =
    model.actualWidth


actualHeight : Model -> Int
actualHeight model =
    model.actualHeight
