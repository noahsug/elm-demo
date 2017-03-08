module View exposing (view)

import Collage
import Color
import Config exposing (gridSize)
import Element
import Game.Model exposing (Entity, Action(..))
import Game.State exposing (directionXY)
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
            ([ drawHero model ]
                ++ List.map (drawCreep model) model.game.creeps
                ++ List.map (drawBlock model) model.game.blocks
            )
        )


drawHero : Model -> Collage.Form
drawHero model =
    Collage.circle (Screen.toActual model.screen (gridSize / 2))
        |> Collage.filled Color.black
        |> Collage.move
            ( entityX model model.game.hero, entityY model model.game.hero )


drawCreep : Model -> Entity -> Collage.Form
drawCreep model creep =
    Collage.circle (Screen.toActual model.screen (gridSize / 2))
        |> Collage.filled Color.red
        |> Collage.move
            ( entityX model creep, entityY model creep )


drawBlock : Model -> Entity -> Collage.Form
drawBlock model block =
    Collage.ngon 4 (Screen.toActual model.screen (1.4 *  gridSize / 2))
        |> Collage.filled Color.gray
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model block, entityY model block )


entityX : Model -> Entity -> Float
entityX model entity =
    let
        dx =
            Tuple.first (directionXY entity.direction)

        x =
            case entity.action of
                MOVE ->
                    interpolate model (toFloat dx) (toFloat entity.x)

                _ ->
                    toFloat entity.x
    in
        gridToActual model x


entityY : Model -> Entity -> Float
entityY model entity =
    let
        dy =
            Tuple.second (directionXY entity.direction)

        y =
            case entity.action of
                MOVE ->
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



-- drawHero : Model -> Collage.Form
-- drawHero model =
--     Collage.circle (Screen.toActual model.screen 15)
--         |> Collage.filled Color.black
--         |> Collage.move
--             ( Screen.toActual model.screen 0
--             , Screen.toActual model.screen 0
--             )
--         |> Collage.rotate (degrees 45)
--
--
-- drawCreep : Model -> Entity.Model -> Collage.Form
-- drawCreep model entity =
--     Collage.circle (Screen.toActual model.screen 15)
--         |> Collage.filled Color.red
--         |> Collage.move
--             ( Screen.toActual model.screen entity.x
--             , Screen.toActual model.screen entity.y
--             )
