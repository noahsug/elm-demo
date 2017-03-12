module Game.State exposing (init, update)

import Game.Creep as Creep
import Game.Hero as Hero
import Game.Model exposing (..)
import Game.Utils exposing (facing)
import List.Extra


init : Model
init =
    { hero = Hero.create
    , creeps = [ Creep.create ]
    , blocks = []
    }


update : Model -> Model
update model =
    model
        |> execute
        |> pick
        |> resolve


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
    { action = NoAction
    , direction = Down
    , x = x
    , y = y
    , kind = Block
    }


pick : Model -> Model
pick model =
    { model
        | hero = Hero.act model
        , creeps = List.map (Creep.act model) model.creeps
    }


resolve : Model -> Model
resolve model =
    { model | hero = maybeKillHero model.creeps model.hero }


maybeKillHero : List Entity -> Entity -> Entity
maybeKillHero creeps hero =
    if List.any (\c -> c.action == KillHero) creeps then
        { hero | action = NoAction }
    else
        hero
