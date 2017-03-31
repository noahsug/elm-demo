module Game.Utils exposing (..)

import Game.Model exposing (..)


xyToDirection : Int -> Int -> Direction
xyToDirection x y =
    if x > 0 then
        Right
    else if x < 0 then
        Left
    else if y > 0 then
        Up
    else
        Down


directionToXY : Direction -> ( Int, Int )
directionToXY dir =
    case dir of
        Up ->
            ( 0, 1 )

        Down ->
            ( 0, -1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


directionToEntity : Entity -> Entity -> Direction
directionToEntity target source =
    let
        dx = target.x - source.x
        dy = target.y - source.y
    in
        if abs dx > abs dy then
            xyToDirection dx 0
        else
            xyToDirection 0 dy


facing : Entity -> ( Int, Int )
facing entity =
    let
        ( x, y ) =
            directionToXY entity.direction
    in
        ( entity.x + x, entity.y + y )


distanceFromEntity : Entity -> Entity -> Int
distanceFromEntity e1 e2 =
    abs (e1.x - e2.x) + abs (e1.y - e2.y)


distanceFromCenter : Entity -> Int
distanceFromCenter entity =
    (abs entity.x) + (abs entity.y)


position : Entity -> ( Int, Int )
position entity =
    ( entity.x, entity.y )


nextPosition : Entity -> ( Int, Int )
nextPosition entity =
    case entity.action of
        Move ->
            facing entity

        _ ->
            position entity


isStructure : Entity -> Bool
isStructure entity =
    case entity.kind of
        Structure _ ->
            True

        _ ->
            False


isTurret : Entity -> Bool
isTurret entity =
    case entity.kind of
        Structure Turret ->
            True

        _ ->
            False


isBlock : Entity -> Bool
isBlock entity =
    case entity.kind of
        Structure Block ->
            True

        _ ->
            False
