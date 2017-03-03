port module Main exposing (..)

import AnimationFrame
import Collage
import Color
import Element
import Html
import Screen
import Task
import Time
import Window


speed : number
speed =
    100


spawnX : number
spawnX =
    -250


spawnY : number
spawnY =
    0


spawnRotation : number
spawnRotation =
    0


heroX = 0
heroY = 0

rotationRate : Float
rotationRate =
    0.25


dt : Float
dt =
    0.03


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { hero : Entity
    , creeps : List Entity
    , timeUntilSpawn : Float
    , screen : Screen.Model
    }


type alias Entity =
    { x : Float
    , y : Float
    , rotation : Float
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { hero = Entity heroX heroY 0
      , creeps = []
      , timeUntilSpawn = 0
      , screen = Screen.init
      }
    , Task.perform Resize Window.size
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        ]



-- UPDATE


type Msg
    = Tick Time.Time
    | Resize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
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
            ( True, timeLeft + 1 )
        else
            ( False, timeLeft )


spawnCreep : Entity
spawnCreep =
    { x = spawnX, y = spawnY, rotation = spawnRotation }


moveHero : Float -> Model -> Model
moveHero dt model =
    { model
        | hero =
            model.hero
                |> rotateHero dt
                |> move dt
    }


moveCreeps : Float -> Model -> Model
moveCreeps dt model =
    { model
        | creeps =
            model.creeps
                |> List.map (move dt << rotateTowards dt model.hero)
    }


rotateHero : Float -> Entity -> Entity
rotateHero dt entity =
    let
        maxRotation =
            rotationRate * (2 * pi * dt)

        newRotation =
            entity.rotation + maxRotation
    in
        { entity | rotation = newRotation }


rotateTowards : Float -> Entity -> Entity -> Entity
rotateTowards dt target entity =
    let
        dx =
            target.x - entity.x

        dy =
            target.y - entity.y

        desiredRotation =
            atan (dy / dx)
                + if dx < 0 then
                    pi
                  else
                    0

        maxChange =
            rotationRate * (2 * pi * dt)

        desiredChange =
            angleDiff desiredRotation entity.rotation

        newRotation =
            entity.rotation
                + if abs desiredChange > maxChange then
                    normalizeTo maxChange desiredChange
                  else
                    desiredChange
    in
        { entity | rotation = newRotation }


move : Float -> Entity -> Entity
move dt entity =
    let
        distance =
            speed * dt

        dx =
            distance * cos entity.rotation

        dy =
            distance * sin entity.rotation
    in
        { entity | x = entity.x + dx, y = entity.y + dy }


log : a -> b -> b
log msg value =
    Debug.log (toString msg) value


sign : Float -> Int
sign value =
    if value < 0 then
        -1
    else
        1


normalizeTo : Float -> Float -> Float
normalizeTo normal value =
    if value < 0 then
        -normal
    else
        normal


angleDiff : Float -> Float -> Float
angleDiff target angle =
    let
        da =
            normalizeAngle (target - angle)
    in
        if da > pi then
            da - 2 * pi
        else
            da


normalizeAngle : Float -> Float
normalizeAngle angle =
    let
        turns =
            toFloat <| floor <| angle / (2 * pi)
    in
        angle - turns * 2 * pi



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.toHtml
        (Collage.collage
            (Screen.actualWidth model.screen)
            (Screen.actualHeight model.screen)
            (drawHero model :: List.map (drawCreep model) model.creeps)
        )


drawHero : Model -> Collage.Form
drawHero model =
    Collage.circle (Screen.toActual model.screen 15)
        |> Collage.filled Color.black
        |> Collage.move
            ( Screen.toActual model.screen model.hero.x
            , Screen.toActual model.screen model.hero.y
            )
        |> Collage.rotate (degrees 45)


drawCreep : Model -> Entity -> Collage.Form
drawCreep model entity =
    Collage.circle (Screen.toActual model.screen 15)
        |> Collage.filled Color.red
        |> Collage.move
            ( Screen.toActual model.screen entity.x
            , Screen.toActual model.screen entity.y
            )
