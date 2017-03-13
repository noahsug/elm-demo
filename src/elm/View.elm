module View exposing (view)

import Collage
import Color
import Config exposing (gridSize, heroRadius)
import Element
import Game.Model exposing (Entity, Action(..))
import Game.Utils exposing (directionToXY)
import Html
import Model exposing (Model)
import Msg exposing (..)
import Screen


view : Model -> Html.Html Msg
view model =
    Element.toHtml
        (Collage.collage
            (Screen.actualWidth model.screen)
            (Screen.actualHeight model.screen)
            (drawBackground model
                ++ drawHero model
                ++ List.map (drawCreep model) model.game.creeps
                ++ List.map (drawBlock model) model.game.blocks
            )
        )


drawBackground : Model -> List Collage.Form
drawBackground model =
    -- Background Color
    [ Collage.rect
        (toFloat (Screen.actualWidth model.screen))
        (toFloat (Screen.actualHeight model.screen))
        |> Collage.filled (Color.rgb 21 27 31)
      -- Hero Radius
    , Collage.circle
        (Screen.toActual model.screen
            (gridSize * (toFloat heroRadius))
        )
        |> Collage.filled (Color.rgba 255 255 255 0.1)
    ]


drawHero : Model -> List Collage.Form
drawHero model =
    [ Collage.circle
        (Screen.toActual model.screen
            (gridSize / 2)
        )
        |> Collage.filled Color.gray
        |> Collage.move
            ( entityX model model.game.hero, entityY model model.game.hero )
    ]


drawCreep : Model -> Entity -> Collage.Form
drawCreep model creep =
    Collage.circle
        (Screen.toActual model.screen
            (gridSize / 2)
        )
        |> Collage.filled (Color.rgb 255 112 67)
        |> Collage.move
            ( entityX model creep, entityY model creep )


drawBlock : Model -> Entity -> Collage.Form
drawBlock model block =
    Collage.ngon 4
        (Screen.toActual model.screen
            (1.4 * gridSize / 2)
        )
        |> Collage.filled (Color.rgb 141 110 99)
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model block, entityY model block )


entityX : Model -> Entity -> Float
entityX model entity =
    let
        dx =
            Tuple.first (directionToXY entity.direction)

        x =
            case entity.action of
                Move ->
                    interpolate model (toFloat dx) (toFloat entity.x)

                _ ->
                    toFloat entity.x
    in
        gridToActual model x


entityY : Model -> Entity -> Float
entityY model entity =
    let
        dy =
            Tuple.second (directionToXY entity.direction)

        y =
            case entity.action of
                Move ->
                    interpolate model (toFloat dy) (toFloat entity.y)

                _ ->
                    toFloat entity.y
    in
        gridToActual model y


interpolate : Model -> Float -> Float -> Float
interpolate model amount value =
    value + amount * (1 - model.timeUntilGameUpdate / Config.gameUpdateTime)


gridToActual : Model -> Float -> Float
gridToActual model gridValue =
    gridValue
        |> (*) gridSize
        |> Screen.toActual model.screen
