port module Main exposing (..)

import AnimationFrame
import Config
import Game.Model as Game
import Game.State
import Html
import Input
import Model exposing (..)
import Msg exposing (..)
import Screen
import Task
import Time
import View exposing (view)
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
    if model.runUntil > 0 then
        Sub.batch
            [ AnimationFrame.diffs Tick
            , Window.resizes Resize
            ]
    else
        Sub.none


init : ( Model, Cmd Msg )
init =
    ( { timeUntilGameUpdate = 0
      , game = Game.State.init
      , screen = Screen.init
      , state = Playing
      , runUntil = Config.gameUpdateTime * 5
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( { model | screen = Screen.resize size model.screen }, Cmd.none )

        _ ->
            case model.state of
                Playing ->
                    updatePlaying msg model

                _ ->
                    ( model, Cmd.none )


updatePlaying : Msg -> Model -> ( Model, Cmd Msg )
updatePlaying msg model =
    case msg of
        Tick time ->
            let
                dt =
                    min (Time.inSeconds time) 0.1

                ( timeUntilGameUpdate, game ) =
                    maybeUpdateGame dt model

                state =
                    gameState game
            in
                ( { model
                    | game = game
                    , timeUntilGameUpdate = timeUntilGameUpdate
                    , runUntil = model.runUntil - dt
                    , state = state
                  }
                , Cmd.none
                )

        Click position ->
            ( { model
                | game =
                    Game.State.spawnCreep
                        (creepXY model position)
                        model.game
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


gameState : Game.Model -> State
gameState game =
    if game.gameOver then
        Win
    else if game.ticks >= Config.ticksUntilGameOver then
        Lose
    else
        Playing


maybeUpdateGame : Float -> Model -> ( Float, Game.Model )
maybeUpdateGame dt model =
    let
        timeUntilGameUpdate =
            model.timeUntilGameUpdate - dt
    in
        if timeUntilGameUpdate <= 0 then
            ( timeUntilGameUpdate + Config.gameUpdateTime
            , Game.State.update model.game
            )
        else
            ( timeUntilGameUpdate, model.game )


creepXY : Model -> Input.Position -> ( Int, Int )
creepXY model position =
    let
        dx =
            position.x - Screen.actualWidth model.screen // 2

        dy =
            position.y - Screen.actualHeight model.screen // 2
    in
        if abs dx > abs dy then
            if dx > 0 then
                ( Config.heroRadius, 0 )
            else
                ( -Config.heroRadius, 0 )
        else if dy > 0 then
            ( 0, -Config.heroRadius )
        else
            ( 0, Config.heroRadius )
