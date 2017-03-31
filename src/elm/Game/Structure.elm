module Game.Structure exposing (create, makeChoice)

import Game.Model exposing (..)
import Game.Utils exposing (distanceFromEntity, directionToEntity)
import List.Extra


create : StructureType -> Int -> Int -> Entity
create structureType x y =
    let
        health =
            case structureType of
                Block ->
                    3

                Turret ->
                    1
    in
        { action = NoAction
        , direction = Down
        , x = x
        , y = y
        , px = x
        , py = y
        , kind = Structure structureType
        , health = health
        }


makeChoice : Model -> Entity -> Entity
makeChoice model structure =
    case structure.kind of
        Structure Turret ->
            attackCreeps model.creeps structure

        _ ->
            structure


attackCreeps : List Entity -> Entity -> Entity
attackCreeps creeps turret =
    let
        target =
            List.Extra.find
                (\creep -> distanceFromEntity creep turret <= 2)
                creeps
    in
        case target of
            Just creep ->
                { turret
                    | action = Attack creep
                    , direction = directionToEntity creep turret
                }

            Nothing ->
                { turret | action = NoAction }
