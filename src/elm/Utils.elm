module Utils exposing (..)


angleDiff : Float -> Float -> Float
angleDiff target angle =
    let
        da =
            normalizeAngle (target - angle)
    in
        if da > pi then
            da - 2 * pi
        else
            da


normalizeAngle : Float -> Float
normalizeAngle angle =
    let
        turns =
            toFloat <| floor <| angle / (2 * pi)
    in
        angle - turns * 2 * pi


normalizeTo : Float -> Float -> Float
normalizeTo normal value =
    if value < 0 then
        -normal
    else
        normal


signf : Float -> Int
signf value =
    if value < 0 then
        -1
    else
        1


sign : Int -> Int
sign value =
    if value < 0 then
        -1
    else
        1


normalize : Int -> Int
normalize value =
    if value == 0 then
        0
    else
        sign value
