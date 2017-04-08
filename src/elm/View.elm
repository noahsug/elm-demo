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
import Game.Update
import Html exposing (div, h1, h3, text)
import Html.Attributes exposing (class)
import Input
import Model exposing (..)
import Msg exposing (..)
import Screen
import Text
import Animation


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
        (gridToActual model (toFloat heroRadius))
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
                        gridToActual model (toFloat i + xPadding)

                    y =
                        gridToActual model (toFloat Config.heroRadius + 1)
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
    let
        ( offsetX, offsetY ) =
            animateBuild model model.game.hero
    in
        Collage.ngon 4
            (gridToActual model (1 / 3))
            |> Collage.filled Color.white
            |> Collage.rotate (degrees 45)
            |> Collage.move
                ( entityX model model.game.hero + offsetX
                , entityY model model.game.hero + offsetY
                )


drawCreep : Model -> Entity -> CreepType -> Collage.Form
drawCreep model creep kind =
    let
        ( xOffset, yOffset ) =
            animateAttack model creep
    in
        creepForm model creep 1
            |> Collage.move
                ( entityX model creep + xOffset
                , entityY model creep + yOffset
                )


creepForm : Model -> Entity -> Float -> Collage.Form
creepForm model creep scale =
    let
        kind =
            case creep.kind of
                Creep kind ->
                    kind

                _ ->
                    Tank

        baseColor =
            case kind of
                Tank ->
                    tankColor

                Dmg ->
                    dmgColor

        shape =
            Collage.circle
                (gridToActual model (scale / 3))

        color =
            (animateCreepDmg
                baseColor
                model
                creep
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
        (gridToActual model 0.7)
        |> Collage.filled
            (animateStructureDmg
                blockColor
                model
                structure
            )
        |> Collage.rotate (degrees 45)
        |> Collage.move
            ( entityX model structure, entityY model structure )


drawTurret : Model -> Entity -> Collage.Form
drawTurret model structure =
    let
        color =
            animateTurretShot model structure
    in
        Collage.ngon 4
            (gridToActual model 0.5)
            |> Collage.filled color
            |> Collage.move
                ( entityX model structure, entityY model structure )


drawAoeTurret : Model -> Entity -> Collage.Form
drawAoeTurret model structure =
    drawTurret model structure


entityX : Model -> Entity -> Float
entityX model entity =
    let
        dx =
            Tuple.first (directionToXY entity.direction)

        x =
            toFloat entity.x
                + case entity.action of
                    Move amount ->
                        animateLinear (toFloat <| dx * amount) model

                    _ ->
                        0
    in
        gridToActual model x


entityY : Model -> Entity -> Float
entityY model entity =
    let
        dy =
            Tuple.second (directionToXY entity.direction)

        y =
            toFloat entity.y
                + case entity.action of
                    Move amount ->
                        animateLinear (toFloat <| dy * amount) model

                    _ ->
                        0
    in
        gridToActual model y


gridToActual : Model -> Float -> Float
gridToActual model gridValue =
    gridValue
        |> (*) gridSize
        |> Screen.toActual model.screen


animateLinear : Float -> Model -> Float
animateLinear amount model =
    animate (Animation.linear amount) model


animateTurretShot : Model -> Entity -> Color.Color
animateTurretShot model entity =
    case entity.action of
        Attack _ ->
            let
                { red, green, blue } =
                    Color.toRgb turretColor

                ratio =
                    animate (Animation.step 0.8 1 0.8) model
            in
                Color.rgb
                    (brightenValue ratio red)
                    (brightenValue ratio green)
                    (brightenValue ratio blue)

        _ ->
            turretColor


animateBuild : Model -> Entity -> ( Float, Float )
animateBuild model entity =
    case entity.action of
        Build _ ->
            let
                amount =
                    gridToActual model 0.2

                animation =
                    animate
                        (Animation.combine
                            0.5
                            (Animation.linear 0)
                            (Animation.inAndOut
                                0.5
                                Animation.linear
                                amount
                            )
                        )
                        model

                ( dx, dy ) =
                    directionToXY entity.direction
            in
                ( toFloat dx * animation, toFloat dy * animation )

        _ ->
            ( 0, 0 )


animateAttack : Model -> Entity -> ( Float, Float )
animateAttack model entity =
    --case entity.action of
    --    Attack _ ->
    --        let
    --            amount =
    --                gridToActual model 0.3
    --
    --            animation =
    --                animate
    --                    (Animation.combine
    --                        0.25
    --                        (Animation.linear 0)
    --                        (Animation.combine
    --                            0.5
    --                            (Animation.easeIn amount)
    --                            (Animation.easeOut -amount)
    --                        )
    --                    )
    --                    model
    --
    --            ( dx, dy ) =
    --                directionToXY entity.direction
    --        in
    --            ( toFloat dx * animation, toFloat dy * animation )
    --
    --    _ ->
    --        ( 0, 0 )
    ( 0, 0 )


animateStructureDmg : Color.Color -> Model -> Entity -> Color.Color
animateStructureDmg color model entity =
    let
        maxHealth =
            case entity.kind of
                Structure Block ->
                    3

                _ ->
                    0

        dmg =
            (maxHealth - toFloat entity.health) / maxHealth

        { red, green, blue } =
            Color.toRgb color

        alpha =
            1 - (1 / maxHealth) * dmg
    in
        Color.rgba red green blue alpha


animateCreepDmg : Color.Color -> Model -> Entity -> Color.Color
animateCreepDmg color model entity =
    let
        maxHealth =
            case entity.kind of
                Creep Tank ->
                    4

                Creep Dmg ->
                    2

                _ ->
                    0

        ratio =
            0.8 * (1 - (toFloat entity.health) / maxHealth)

        { red, green, blue } =
            Color.toRgb color
    in
        Color.rgb
            (brightenValue ratio red)
            (darkenValue ratio green)
            (darkenValue ratio blue)


animate : (Float -> Float) -> Model -> Float
animate animation model =
    animation (1 - model.timeUntilGameUpdate / Config.gameUpdateTime)


brightenValue : Float -> Int -> Int
brightenValue ratio color =
    let
        value =
            toFloat color
    in
        round <| value + (255 - value) * ratio


darkenValue : Float -> Int -> Int
darkenValue ratio color =
    brightenValue -ratio color
