module Game.State exposing (init, update, spawnCreep)

import Config
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
        [ Creep.createTank
        , Creep.createDmg
        , Creep.createDmg
        , Creep.createDmg
        , Creep.createTank
        , Creep.createTank
        ]
    , creepsSpawned = 0
    , gameOver = False
    , ticks = 0
    }


update : Model -> Model
update model =
    model
        --|> spawnCreeps
        |> Update.execute
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
    if model.creepsSpawned * Config.creepReadyRate <= model.ticks then
        case model.creepLine of
            spawn :: line ->
                let
                    ( x, y ) =
                        pos

                    creep =
                        { spawn | x = x, y = y }
                in
                    { model
                        | creeps = creep :: model.creeps
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
            creep =
                case (model.ticks // 3) % 8 of
                    0 ->
                        Creep.create 0 10

                    1 ->
                        Creep.create 0 -10

                    2 ->
                        Creep.create 10 0

                    3 ->
                        Creep.create -10 0

                    4 ->
                        Creep.create 7 7

                    5 ->
                        Creep.create 7 -7

                    6 ->
                        Creep.create -7 7

                    _ ->
                        Creep.create -7 -7
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
