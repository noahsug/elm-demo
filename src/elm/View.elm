module View exposing (view)

import Collage
import Color
import Config exposing (gridSize, heroRadius, ticksUntilGameOver)
import Element
import Game.Model
    exposing
        ( Entity
        , EntityType(..)
        , StructureType(..)
        , CreepType(..)
        , Action(..)
        )
import Game.Utils exposing (directionToXY, numSpawnableCreeps)
import Html exposing (div, h1, h3, text)
import Html.Attributes exposing (class)
import Input
import Model exposing (..)
import Msg exposing (..)
import Screen
import Text


-- Dark Bg: 0 168 169
-- Light Bg: 2 186 191
-- Yellow: 233 186 32
-- 25% Yellow: 239 203 88
-- Tan: 239 215 201
-- 25% Light Bg: 191 238 239
-- 50% Light Bg: 128 220 223
-- Pink: 243 162 142
-- 25% Pink: 246 185 170
-- Dark Pink: 231 144 127


heroRadiusColor =
    Color.rgb 2 186 191


turretColor =
    Color.rgb 191 238 239


blockColor =
    Color.rgb 128 220 223


dmgColor =
    Color.rgb 246 185 170


tankColor =
    Color.rgb 239 203 88


outlineStyle =
    { color = Color.rgba 255 255 255 0.25
    , width = 3
    , cap = Collage.Flat
    , join = Collage.Smooth
    , dashing = []
    , dashOffset = 0
    }


view : Model -> Html.Html Msg
view model =
    Html.section
        [ Input.onClick Click ]
        [ Element.toHtml
            (Collage.collage
                (Screen.actualWidth model.screen)
                (Screen.actualHeight model.screen)
                (drawBackground model
                    ++ [ drawEntity model model.game.hero ]
                    ++ List.map (drawEntity model) model.game.creeps
                    ++ List.map (drawEntity model) model.game.structures
                    ++ maybeDrawCreepLine model
                )
            )
        , drawTextOverlay model
        ]


drawTextOverlay : Model -> Html.Html Msg
drawTextOverlay model =
    case model.state of
        Lost ->
            div
                [ class "overlay game-over" ]
                [ h1 [] [ text "You Lose" ]
                , h3 [] [ text "click to try again" ]
                ]

        Won ->
            div
                [ class "overlay game-over" ]
                [ h1 [] [ text "You Win!" ]
                , h3 [] [ text "click to play again" ]
                ]

        Intro ->
            div
                [ class "overlay" ]
                [ h1 [] [ text "Kill the Square" ]
                , h3 [] [ text "click to start" ]
                ]

        Playing ->
            div
                [ class "overlay fade" ]
                []


drawBackground : Model -> List Collage.Form
drawBackground model =
    [ Collage.circle
        (Screen.toActual model.screen
            (gridSize * (toFloat heroRadius))
        )
        |> Collage.filled heroRadiusColor
    ]


drawTicks : Model -> Collage.Form
drawTicks model =
    Text.fromString (toString (ticksUntilGameOver - model.game.ticks))
        |> Text.color Color.white
        |> Text.typeface [ "helvetica", "arial", "sans-serif" ]
        |> Element.centered
        |> Collage.toForm
        |> Collage.move
            ( toFloat <| 0
            , toFloat <| Screen.actualHeight model.screen // 2 - 20
            )


maybeDrawCreepLine : Model -> List Collage.Form
maybeDrawCreepLine model =
    case model.state of
        Playing ->
            drawCreepLine model

        _ ->
            []


drawCreepLine : Model -> List Collage.Form
drawCreepLine model =
    model.game.creepLine
        |> List.indexedMap
            (\i creep ->
                let
                    scale =
                        if i == 0 then
                            2
                        else if i == 1 then
                            1.5
                        else
                            1

                    xPadding =
                        if i == 0 then
                            0
                        else if i == 1 then
                            0.5
                        else
                            0.65

                    x =
                        Screen.toActual model.screen gridSize * (toFloat i + xPadding)

                    y =
                        Screen.toActual model.screen gridSize
                            * toFloat (Config.heroRadius + 1)
                in
                    creepForm model creep scale
                        |> Collage.move ( x, y )
            )


drawContinue : Model -> Collage.Form
drawContinue model =
    Text.fromString "click to continue"
        |> Text.color Color.white
        |> Text.typeface [ "helvetica", "arial", "sans-serif" ]
        |> Element.centered
        |> Collage.toForm
        |> Collage.move
            ( toFloat <| 0
            , toFloat <| Screen.actualHeight model.screen // -4
            )


drawFade : Model -> Collage.Form
drawFade model =
    Collage.rect
        (toFloat (Screen.actualWidth model.screen))
        (toFloat (Screen.actualHeight model.screen))
        |> Collage.filled (Color.rgba 21 27 31 0.5)


drawEntity : Model -> Entity -> Collage.Form
drawEntity model entity =
    case entity.kind of
        Hero ->
            drawHero model

        Creep kind ->
            drawCreep model entity kind

        Structure Block ->
            drawBlock model entity

        Structure Turret ->
            drawTurret model entity

        Structure AoeTurret ->
            drawAoeTurret model entity


drawHero : Model -> Collage.Form
drawHero model =
    Collage.ngon 4
        (Screen.toActual model.screen
            (gridSize / 3)
        )
        |> Collage.filled Color.white
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model model.game.hero, entityY model model.game.hero )


drawCreep : Model -> Entity -> CreepType -> Collage.Form
drawCreep model creep kind =
    creepForm model creep 1
        |> Collage.move ( entityX model creep, entityY model creep )


creepForm : Model -> Entity -> Float -> Collage.Form
creepForm model creep scale =
    let
        kind =
            case creep.kind of
                Creep kind ->
                    kind

                _ ->
                    Tank

        color =
            case kind of
                Tank ->
                    tankColor

                Dmg ->
                    dmgColor

        shape =
            Collage.circle
                (Screen.toActual model.screen
                    (scale * gridSize / 3)
                )
    in
        --[ shape
        --    |> Collage.filled color
        --, shape
        --    |> Collage.outlined outlineStyle
        --]
        --    |> Collage.group
        shape
            |> Collage.filled color


drawBlock : Model -> Entity -> Collage.Form
drawBlock model structure =
    Collage.ngon 4
        (Screen.toActual model.screen
            (1.4 * gridSize / 2)
        )
        |> Collage.filled blockColor
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model structure, entityY model structure )


drawTurret : Model -> Entity -> Collage.Form
drawTurret model structure =
    let
        { red, green, blue } =
            Color.toRgb turretColor

        animatedRed =
            case structure.action of
                Attack _ ->
                    red
                        |> toFloat
                        |> stepInterpolation model -100 0.8 1
                        |> round

                _ ->
                    red
    in
        Collage.ngon 4
            (Screen.toActual model.screen gridSize / 2)
            |> Collage.filled (Color.rgb animatedRed green blue)
            |> Collage.move
                ( entityX model structure, entityY model structure )


drawAoeTurret : Model -> Entity -> Collage.Form
drawAoeTurret model structure =
    let
        baseColor =
            147

        primaryColor =
            case structure.action of
                Attack _ ->
                    baseColor
                        |> stepInterpolation model 100 0.8 1
                        |> round

                _ ->
                    baseColor
    in
        Collage.ngon 5
            (Screen.toActual model.screen gridSize / 2)
            |> Collage.filled (Color.rgb primaryColor primaryColor 90)
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
                Move amount ->
                    linearInterpolation model
                        (toFloat <| dx * amount)
                        (toFloat entity.x)

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
                Move amount ->
                    linearInterpolation model
                        (toFloat <| dy * amount)
                        (toFloat entity.y)

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
        time =
            1 - model.timeUntilGameUpdate / Config.gameUpdateTime
    in
        if time > start && time < end then
            value + amount
        else
            value


gridToActual : Model -> Float -> Float
gridToActual model gridValue =
    gridValue
        |> (*) gridSize
        |> Screen.toActual model.screen
