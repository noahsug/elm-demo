module Game.Structure exposing (create, makeChoice)

import Game.Model exposing (..)
import Game.Utils exposing (distanceFromEntity, directionToEntity, createEntity)
import List.Extra


create : StructureType -> Entity
create structureType =
    let
        { health, dmg } =
            case structureType of
                Block ->
                    { health = 6, dmg = 0 }

                Turret ->
                    { health = 2, dmg = 2 }
    in
        { createEntity
            | kind = Structure structureType
            , health = health
            , dmg = dmg
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
