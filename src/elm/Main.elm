port module Main exposing (..)

import AnimationFrame
import Config
import Game.Model as Game
import Game.State
import Html
import Model exposing (Model)
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
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        ]


init : ( Model, Cmd Msg )
init =
    ( { timeUntilGameUpdate = 0
      , game = Game.State.init
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

                ( timeUntilGameUpdate, game ) =
                    maybeUpdateGame dt model
            in
                ( { model
                    | game = game
                    , timeUntilGameUpdate = timeUntilGameUpdate
                  }
                , Cmd.none
                )

        Resize size ->
            ( { model | screen = Screen.resize size model.screen }, Cmd.none )


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
