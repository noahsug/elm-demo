module Game.State exposing (init, update, spawnCreep)

import Config
import Game.Utils exposing (numSpawnableCreeps)
import Game.Creep as Creep
import Game.Hero as Hero
import Game.Model exposing (..)
import Game.Structure as Structure
import Game.Update as Update


init : Model
init =
    { hero = Hero.create
    , creeps = []
    , structures = []
    , creepLine =
        [ Creep.create Red Tank
        , Creep.create Blue Tank
        , Creep.create Blue Tank
        , Creep.create Green Tank
        , Creep.create Red Tank
        , Creep.create Green Tank
        , Creep.create Red Tank
        , Creep.create Blue Tank
        , Creep.create Blue Tank
        , Creep.create Green Tank
        , Creep.create Green Tank
        , Creep.create Green Tank
        , Creep.create Blue Tank
        , Creep.create Red Tank
        , Creep.create Red Tank
        , Creep.create Red Tank
        , Creep.create Blue Tank
        ]
    , creepsSpawned = 0
    , gameOver = False
    , ticks = 0
    }


update : Model -> Model
update model =
    model
        --|> spawnCreeps
        |>
            Update.execute
        |> makeHeroChoice
        |> makeCreepChoices
        |> makeStructureChoices
        |> Update.resolve
        |> tick


tick : Model -> Model
tick model =
    { model | ticks = model.ticks + 1 }


spawnCreep : ( Int, Int ) -> Model -> Model
spawnCreep pos model =
    if numSpawnableCreeps model > 0 then
        case model.creepLine of
            spawn :: line ->
                let
                    ( x, y ) =
                        pos

                    creep =
                        { spawn | x = x, y = y }
                in
                    { model
                        | creeps = model.creeps ++ [ creep ]
                        , creepLine = line
                        , creepsSpawned = model.creepsSpawned + 1
                    }

            _ ->
                model
    else
        model


spawnCreeps : Model -> Model
spawnCreeps model =
    if model.ticks % 3 /= 0 then
        model
    else
        let
            ( spawn, x, y ) =
                case (model.ticks // 3) % 8 of
                    0 ->
                        ( Creep.create Red Tank, 0, 10 )

                    1 ->
                        ( Creep.create Red Tank, 0, -10 )

                    2 ->
                        ( Creep.create Red Tank, 10, 0 )

                    3 ->
                        ( Creep.create Red Tank, -10, 0 )

                    4 ->
                        ( Creep.create Red Tank, 7, 7 )

                    5 ->
                        ( Creep.create Red Tank, 7, -7 )

                    6 ->
                        ( Creep.create Red Tank, -7, 7 )

                    _ ->
                        ( Creep.create Red Tank, -7, -7 )

            creep =
                { spawn | x = x, y = y }
        in
            { model | creeps = creep :: model.creeps }


makeHeroChoice : Model -> Model
makeHeroChoice model =
    let
        modelHero =
            model.hero

        hero =
            if model.gameOver then
                { modelHero | action = NoAction }
            else
                Hero.makeChoice model
    in
        { model | hero = hero }


makeCreepChoices : Model -> Model
makeCreepChoices model =
    { model | creeps = List.map (Creep.makeChoice model) model.creeps }


makeStructureChoices : Model -> Model
makeStructureChoices model =
    { model | structures = List.map (Structure.makeChoice model) model.structures }
