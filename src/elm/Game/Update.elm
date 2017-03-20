module Game.Update exposing (execute, resolve)

import Game.Structure as Structure
import Game.Model exposing (..)
import Game.Utils exposing (facing, nextPosition, distanceFromEntity)


execute : Model -> Model
execute model =
    let
        aliveStructures =
            killStructures model

        hero =
            move model.hero

        creeps =
            List.map move model.creeps

        spawnedStructures =
            spawnStructures creeps model.hero

        gameOver =
            model.gameOver || isGameOver creeps hero
    in
        { model
            | hero = hero
            , creeps = creeps
            , structures = aliveStructures ++ spawnedStructures
            , gameOver = gameOver
        }


killStructures : Model -> List Entity
killStructures model =
    model.structures
        |> List.map
            (\b -> { b | health = b.health - (numAttackers model.creeps b) })
        |> List.filter
            (\b -> b.health > 0)


numAttackers : List Entity -> Entity -> Int
numAttackers creeps target =
    creeps
        |> List.filter (isAttackingEntity target)
        |> List.length


isAttackingEntity : Entity -> Entity -> Bool
isAttackingEntity target creep =
    case creep.action of
        Attack ->
            facing creep == ( target.x, target.y )

        _ ->
            False


spawnStructures : List Entity -> Entity -> List Entity
spawnStructures creeps hero =
    let
        ( x, y ) =
            facing hero
    in
        case hero.action of
            Build ->
                -- If a creep moved into where hero is building, destroy that
                -- building.
                if List.any (\c -> c.x == x && c.y == y) creeps then
                    []
                else
                    [ createBlock x y ]

            _ ->
                []


createBlock : Int -> Int -> Entity
createBlock x y =
    let
        block =
            Structure.createBlock
    in
        { block | x = x, y = y }


move : Entity -> Entity
move entity =
    let
        ( x, y ) =
            facing entity
    in
        case entity.action of
            Move ->
                { entity | x = x, y = y, px = entity.x, py = entity.y }

            _ ->
                entity


isGameOver : List Entity -> Entity -> Bool
isGameOver creeps hero =
    List.any
        (\creep ->
            distanceFromEntity creep hero == 1
        )
        creeps


resolve : Model -> Model
resolve model =
    model
        |> stopSameSquareMovement


stopSameSquareMovement : Model -> Model
stopSameSquareMovement model =
    let
        entities =
            recursiveStopSameSquareMovement (model.hero :: model.creeps)
    in
        case entities of
            hero :: creeps ->
                { model | hero = hero, creeps = creeps }

            _ ->
                model


recursiveStopSameSquareMovement : List Entity -> List Entity
recursiveStopSameSquareMovement entities =
    case entities of
        _ :: [] ->
            entities

        entity :: entityList ->
            let
                fixedEntityList =
                    recursiveStopSameSquareMovement entityList

                fixedEntity =
                    if willCollide fixedEntityList entity then
                        { entity | action = NoAction }
                    else
                        entity
            in
                fixedEntity :: fixedEntityList

        _ ->
            entities


willCollide : List Entity -> Entity -> Bool
willCollide entityList entity =
    List.any
        (\listEntity -> nextPosition entity == nextPosition listEntity)
        entityList
