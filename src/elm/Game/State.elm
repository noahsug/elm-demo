module Game.State exposing (init, update, directionXY)

import Game.Creep as Creep
import Game.Hero as Hero
import Game.Model exposing (..)


init : Model
init =
    { hero = Hero.create
    , creeps = [ Creep.create ]
    , blocks = []
    }


update : Model -> Model
update model =
    model
        |> resolve
        |> act


resolve : Model -> Model
resolve model =
    let
        hero =
            move model.hero

        creeps =
            List.map move model.creeps

        blocks =
            maybeSpawnBlocks model
    in
        { model
            | hero = hero
            , creeps = creeps
            , blocks = blocks
        }


move : Entity -> Entity
move entity =
    let
        ( dx, dy ) =
            directionXY entity.direction
    in
        case entity.action of
            MOVE ->
                { entity | x = entity.x + dx, y = entity.y + dy }

            _ ->
                entity


maybeSpawnBlocks : Model -> List Entity
maybeSpawnBlocks model =
    let
        ( dx, dy ) =
            directionXY model.hero.direction

        x = model.hero.x + dx

        y = model.hero.y + dy
    in
        model.blocks
            ++ case model.hero.action of
                BUILD ->
                    [ createBlock x y ]

                _ ->
                    []


createBlock : Int -> Int -> Entity
createBlock x y =
    { action = NONE
    , direction = DOWN
    , x = x
    , y = y
    }


act : Model -> Model
act model =
    { model
        | hero = Hero.act model
        , creeps = List.map (Creep.act model) model.creeps
    }


directionXY : Direction -> ( Int, Int )
directionXY dir =
    case dir of
        UP ->
            ( 0, 1 )

        DOWN ->
            ( 0, -1 )

        LEFT ->
            ( -1, 0 )

        RIGHT ->
            ( 1, 0 )



--     let
--         (creeps, ticksUntilSpawn) = Spawn.maybeSpawnCreep model
--     in
--
--         { model
--             , ticksUntilSpawn = ticksUntilSpawn
--             , creeps = List.map (Creep.move model) creeps
--             , hero = Hero.move model }
