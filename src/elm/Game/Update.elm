module Game.Update exposing (execute, resolve, attackerDmg)

import Config
import Game.Structure as Structure
import Game.Model exposing (..)
import Game.Utils exposing (facing, forward, nextPosition, distanceFromEntity)


execute : Model -> Model
execute model =
    let
        structures =
            killEntities model.creeps model.structures
                |> List.map tickAge

        creeps =
            killEntities model.structures model.creeps
                |> List.map move
                |> List.map tickAge

        hero =
            model.hero
                |> move
                |> tickAge

        spawnedStructures =
            spawnStructures creeps model.hero
                |> List.map tickAge

        gameOver =
            model.gameOver || isGameOver creeps hero
    in
        { model
            | hero = hero
            , creeps = creeps
            , structures = structures ++ spawnedStructures
            , gameOver = gameOver
        }


tickAge : Entity -> Entity
tickAge entity =
    { entity | age = entity.age + 1 }


killEntities : List Entity -> List Entity -> List Entity
killEntities attackers entities =
    entities
        |> List.map
            (\e -> { e | health = e.health - (attackerDmg attackers e) })
        |> List.filter
            (\e -> e.health > 0)


attackerDmg : List Entity -> Entity -> Int
attackerDmg attackers target =
    attackers
        |> List.filter (isAttackingEntity target)
        |> List.foldl (\entity dmg -> entity.dmg + dmg) 0


isAttackingEntity : Entity -> Entity -> Bool
isAttackingEntity target entity =
    case entity.action of
        Attack attackTarget ->
            target.x == attackTarget.x && target.y == attackTarget.y

        _ ->
            False


spawnStructures : List Entity -> Entity -> List Entity
spawnStructures creeps hero =
    let
        ( x, y ) =
            facing hero
    in
        case hero.action of
            Build structureType ->
                -- If a creep moved into where hero is building, destroy that
                -- building.
                if List.any (\c -> c.x == x && c.y == y) creeps then
                    []
                else
                    let
                        structure =
                            Structure.create structureType
                    in
                        [ { structure | x = x, y = y } ]

            _ ->
                []


move : Entity -> Entity
move entity =
    case entity.action of
        Move amount ->
            let
                ( x, y ) =
                    forward amount entity
            in
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
