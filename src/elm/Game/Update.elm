module Game.Update exposing (execute, resolve, attackerDmg)

import Config
import Game.Model exposing (..)
import Game.Structure as Structure
import Game.Utils
    exposing
        ( facing
        , canExplode
        , forward
        , position
        , nextPosition
        , distanceFromEntity
        , isDead
        , isAlive
        )
import Game.Grid as Grid
import Utils exposing (normalize)


execute : Model -> Model
execute model =
    let
        dmgedCreeps =
            model.creeps
                |> List.map (takeDmg model.structures)
                |> List.map maybeExplode

        explosions =
            dmgedCreeps
                |> List.filter canExplode
                |> List.filter isDead
                |> List.map position

        structures =
            model.structures
                |> List.map (takeDmg model.creeps)
                |> List.map (takeExplosionDmg explosions)
                |> List.filter isAlive
                |> List.map tickAge

        creeps =
            dmgedCreeps
                |> List.map (takeExplosionDmg explosions)
                |> List.filter isAlive
                |> List.map move
                |> List.map tickAge
                |> List.filter isMostlyInBounds

        hero =
            model.hero
                |> move
                |> tickAge

        spawnedStructures =
            spawnStructures creeps model.hero
                |> List.map tickAge

        gameOver =
            model.gameOver
                || isGameOver creeps hero
                || (explosionDmg explosions model.hero > 0)
    in
        { model
            | hero = hero
            , creeps = creeps
            , structures = structures ++ spawnedStructures
            , gameOver = gameOver
        }


maybeExplode : Entity -> Entity
maybeExplode entity =
    case entity.action of
        Explode ->
            { entity | health = 0 }

        _ ->
            entity


tickAge : Entity -> Entity
tickAge entity =
    { entity | age = entity.age + 1 }


isMostlyInBounds : Entity -> Bool
isMostlyInBounds entity =
    let
        x =
            entity.x - normalize entity.x

        y =
            entity.y - normalize entity.y
    in
        Grid.inBounds x y


takeDmg : List Entity -> Entity -> Entity
takeDmg attackers entity =
    { entity | health = entity.health - (attackerDmg attackers entity) }


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


takeExplosionDmg : List ( Int, Int ) -> Entity -> Entity
takeExplosionDmg explosions entity =
    { entity | health = entity.health - (explosionDmg explosions entity) }


explosionDmg : List ( Int, Int ) -> Entity -> Int
explosionDmg explosions target =
    explosions
        |> List.filter
            (\( x, y ) ->
                abs (x - target.x) <= 1 && abs (y - target.y) <= 1
            )
        |> List.foldl (\entity totalDmg -> 3 + totalDmg) 0


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
