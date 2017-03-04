port module Main exposing (..)

import AnimationFrame
import Config
import Entity
import Html
import Model exposing (..)
import Msg exposing (..)
import Screen
import Task
import Time
import View exposing (..)
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        ]


init : ( Model, Cmd Msg )
init =
    ( { hero = Entity.Model 0 0 0
      , creeps = []
      , timeUntilSpawn = 0
      , screen = Screen.init
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                dt =
                    Time.inSeconds time

                newModel =
                    model
                        |> maybeSpawnCreeps dt
                        |> moveHero dt
                        |> moveCreeps dt
            in
                ( newModel, Cmd.none )

        Resize size ->
            ( { model | screen = Screen.resize size model.screen }, Cmd.none )


maybeSpawnCreeps : Float -> Model -> Model
maybeSpawnCreeps dt model =
    let
        ( shouldSpawn, newTimeUntilSpawn ) =
            shouldSpawnCreeps dt model

        spawn =
            if shouldSpawn then
                [ spawnCreep ]
            else
                []
    in
        { model
            | creeps = model.creeps ++ spawn
            , timeUntilSpawn = newTimeUntilSpawn
        }


shouldSpawnCreeps : Float -> Model -> ( Bool, Float )
shouldSpawnCreeps dt model =
    let
        timeLeft =
            model.timeUntilSpawn - dt
    in
        if timeLeft < 0 then
            ( True, timeLeft + Config.creepSpawnRate )
        else
            ( False, timeLeft )


spawnCreep : Entity.Model
spawnCreep =
    { x = Config.creepSpawnX
    , y = Config.creepSpawnY
    , rotation = 0
    }


moveHero : Float -> Model -> Model
moveHero dt model =
    { model
        | hero =
            model.hero
                |> rotateHero dt
                |> Entity.move (Config.speed * dt)
    }


moveCreeps : Float -> Model -> Model
moveCreeps dt model =
    let
        rotation =
            Config.rotations * (2 * pi * dt)
    in
        { model
            | creeps =
                model.creeps
                    |> List.map
                        (Entity.move (Config.speed * dt)
                            << Entity.rotateTowards
                                rotation
                                model.hero.x
                                model.hero.y
                        )
        }


rotateHero : Float -> Entity.Model -> Entity.Model
rotateHero dt entity =
    let
        maxRotation =
            Config.rotations * (2 * pi * dt)

        newRotation =
            entity.rotation + maxRotation
    in
        { entity | rotation = newRotation }
