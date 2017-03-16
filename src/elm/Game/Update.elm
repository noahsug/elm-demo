module Game.Update exposing (execute, resolve)

import Game.Block as Block
import Game.Model exposing (..)
import Game.Utils exposing (facing, nextPosition)
import List.Extra


execute : Model -> Model
execute model =
    let
        unattackedBlocks =
            removeBlocks model

        hero =
            move model.hero

        creeps =
            List.map move model.creeps

        spawnedBlocks =
            spawnBlocks creeps model.hero
    in
        { model
            | hero = hero
            , creeps = creeps
            , blocks = unattackedBlocks ++ spawnedBlocks
        }


removeBlocks : Model -> List Entity
removeBlocks model =
    List.Extra.filterNot
        (\b ->
            List.any (isAttackingEntity b) model.creeps
        )
        model.blocks


isAttackingEntity : Entity -> Entity -> Bool
isAttackingEntity target creep =
    case creep.action of
        Attack ->
            facing creep == ( target.x, target.y )

        _ ->
            False


spawnBlocks : List Entity -> Entity -> List Entity
spawnBlocks creeps hero =
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
            Block.create
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
                { entity | x = x, y = y }

            _ ->
                entity


resolve : Model -> Model
resolve model =
    model
        |> maybeEndGame
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


maybeEndGame : Model -> Model
maybeEndGame model =
    let
        gameOver =
            model.gameOver
                || isGameOver model.creeps model.hero

        modelHero =
            model.hero

        hero =
            if gameOver then
                { modelHero | action = NoAction }
            else
                modelHero
    in
        { model | hero = hero, gameOver = gameOver }


isGameOver : List Entity -> Entity -> Bool
isGameOver creeps hero =
    List.any (\c -> c.action == KillHero) creeps
