module Game.Utils exposing (..)

import Config
import Game.Model exposing (..)


createEntity : Entity
createEntity =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , px = 0
    , py = 0
    , kind = Hero
    , health = 1
    , dmg = 0
    , color = White
    }


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
        dx =
            target.x - source.x

        dy =
            target.y - source.y
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


distanceFrom : Entity -> Int -> Int -> Int
distanceFrom entity x y =
    abs (entity.x - x) + abs (entity.y - y)


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


numSpawnableCreeps : Model -> Int
numSpawnableCreeps model =
    let
        ready =
            model.ticks
                // Config.creepReadyRate
                - model.creepsSpawned
                + Config.startingCreeps
    in
        min ready (List.length model.creepLine)


bestStructureColor : List Entity -> Int -> Int -> EntityColor
bestStructureColor creeps x y =
    let
        maxDistance = Config.heroRadius * 20 // 14

        (red, green, blue) = List.foldl
            (\creep rgbTotals ->
                 let
                     (red, green, blue) = rgbTotals
                     value = 2 + maxDistance - distanceFrom creep x y
                 in
                     case creep.color of
                         Red ->
                             (red + value, green, blue)
                         Green ->
                             (red, green + value, blue)
                         Blue ->
                             (red, green, blue + value)
                         _ ->
                             rgbTotals
            )
            (0, 0, 0)
            creeps
    in
        if red >= green && red >= blue then
            Blue
        else if green >= blue then
            Red
        else
            Green
