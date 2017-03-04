module Entity exposing (..)

import Utils exposing (..)


type alias Model =
    { x : Float
    , y : Float
    , rotation : Float
    }


rotateTowards : Float -> Float -> Float -> Model -> Model
rotateTowards rotation targetX targetY entity =
    let
        dx =
            targetX - entity.x

        dy =
            targetY - entity.y

        desiredRotation =
            atan (dy / dx)
                + if dx < 0 then
                    pi
                  else
                    0

        desiredChange =
            angleDiff desiredRotation entity.rotation

        newRotation =
            entity.rotation
                + if abs desiredChange > rotation then
                    normalizeTo rotation desiredChange
                  else
                    desiredChange
    in
        { entity | rotation = newRotation }


move : Float -> Model -> Model
move distance entity =
    let
        dx =
            distance * cos entity.rotation

        dy =
            distance * sin entity.rotation
    in
        { entity | x = entity.x + dx, y = entity.y + dy }
