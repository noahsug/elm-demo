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
    , age = 0
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


forward : Int -> Entity -> ( Int, Int )
forward amount entity =
    let
        ( x, y ) =
            directionToXY entity.direction
    in
        ( entity.x + x * amount, entity.y + y * amount )


distanceFromEntity : Entity -> Entity -> Int
distanceFromEntity e1 e2 =
    abs (e1.x - e2.x) + abs (e1.y - e2.y)


distanceFromCenter : Entity -> Int
distanceFromCenter entity =
    (abs entity.x) + (abs entity.y)


distanceFrom : Int -> Int -> Entity -> Int
distanceFrom x y entity =
    abs (entity.x - x) + abs (entity.y - y)


position : Entity -> ( Int, Int )
position entity =
    ( entity.x, entity.y )


nextPosition : Entity -> ( Int, Int )
nextPosition entity =
    case entity.action of
        Move amount ->
            forward amount entity

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


maxPossibleSpawnedCreeps : Model -> Int
maxPossibleSpawnedCreeps model =
    model.ticks
        // Config.creepReadyRate
        - model.creepsSpawned
        + Config.startingCreeps


numSpawnableCreeps : Model -> Int
numSpawnableCreeps model =
    min
        (min
            (maxPossibleSpawnedCreeps model)
            (List.length model.creepLine)
        )
        Config.maxCreepLineLength


closestEntity : List Entity -> Int -> Int -> Maybe ( Int, Entity )
closestEntity entities x y =
    List.foldl
        (\entity maybeClosest ->
            let
                distance =
                    distanceFrom x y entity
            in
                case maybeClosest of
                    Just ( closestDistance, _ ) ->
                        if distance < closestDistance then
                            Just ( distance, entity )
                        else
                            maybeClosest

                    Nothing ->
                        Just ( distance, entity )
        )
        Nothing
        entities


canExplode : Entity -> Bool
canExplode entity =
    case entity.kind of
        Creep Bomb ->
            True

        _ ->
            False


isDead : Entity -> Bool
isDead entity =
    entity.health <= 0


isAlive : Entity -> Bool
isAlive entity =
    entity.health > 0
