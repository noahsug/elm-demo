module View exposing (view)

import Collage
import Color
import Config exposing (gridSize, heroRadius)
import Element
import Game.Model exposing (Entity, EntityType(..), StructureType(..), Action(..))
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
                ++ [ drawEntity model model.game.hero ]
                ++ List.map (drawEntity model) model.game.creeps
                ++ List.map (drawEntity model) model.game.structures
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


drawEntity : Model -> Entity -> Collage.Form
drawEntity model entity =
    case entity.kind of
        Hero ->
            drawHero model

        Creep ->
            drawCreep model entity

        Structure Block ->
            drawBlock model entity

        Structure Turret ->
            drawTurret model entity


drawHero : Model -> Collage.Form
drawHero model =
    Collage.circle
        (Screen.toActual model.screen
            (gridSize / 3)
        )
        |> Collage.filled Color.gray
        |> Collage.move
            ( entityX model model.game.hero, entityY model model.game.hero )


drawCreep : Model -> Entity -> Collage.Form
drawCreep model creep =
    Collage.circle
        (Screen.toActual model.screen
            (gridSize / 3)
        )
        |> Collage.filled (Color.rgb 255 112 67)
        |> Collage.move
            ( entityX model creep, entityY model creep )


drawBlock : Model -> Entity -> Collage.Form
drawBlock model structure =
    Collage.ngon 4
        (Screen.toActual model.screen
            (1.4 * gridSize / 2)
        )
        |> Collage.filled (Color.rgb 141 110 99)
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model structure, entityY model structure )


drawTurret : Model -> Entity -> Collage.Form
drawTurret model structure =
    let
        baseGreen =
            144

        green =
            case structure.action of
                Attack _ ->
                    baseGreen
                        |> stepInterpolation model 100 0.8 1
                        |> round

                _ ->
                    baseGreen
    in
        Collage.ngon 3
            (Screen.toActual model.screen gridSize / 2)
            |> Collage.filled (Color.rgb 110 green 99)
            |> Collage.rotate (degrees 90)
            |> Collage.move
                ( entityX model structure, entityY model structure )


entityX : Model -> Entity -> Float
entityX model entity =
    let
        dx =
            Tuple.first (directionToXY entity.direction)

        x =
            case entity.action of
                Move ->
                    linearInterpolation model (toFloat dx) (toFloat entity.x)

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
                    linearInterpolation model (toFloat dy) (toFloat entity.y)

                _ ->
                    toFloat entity.y
    in
        gridToActual model y


linearInterpolation : Model -> Float -> Float -> Float
linearInterpolation model amount value =
    value + amount * (1 - model.timeUntilGameUpdate / Config.gameUpdateTime)



-- curvedInterpolation : Model -> Float -> Float -> Float
-- curvedInterpolation model amount value =
--     let
--         ratio =
--             1 - model.timeUntilGameUpdate / Config.gameUpdateTime
--
--         a =
--             0
--
--         b =
--             0.2
--     in
--         value
--             + amount
--             * if ratio > a && ratio < b then
--                 sin ((ratio - a) * (1 / b))
--               else
--                 0
--


stepInterpolation : Model -> Float -> Float -> Float -> Float -> Float
stepInterpolation model amount start end value =
    let
        ratio =
            1 - model.timeUntilGameUpdate / Config.gameUpdateTime
    in
        if ratio > start && ratio < end then
            value + amount
        else
            value


gridToActual : Model -> Float -> Float
gridToActual model gridValue =
    gridValue
        |> (*) gridSize
        |> Screen.toActual model.screen
