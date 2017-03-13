module Game.Hero exposing (create, makeChoice)

import Game.Creep as Creep
import Game.Grid as Grid
import Game.Model exposing (..)
import Game.Update as Update
import Game.Utils exposing (directionToXY)


create : Entity
create =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , kind = Hero
    }


makeChoice : Model -> Entity
makeChoice model =
    let
        hero =
            model.hero

        { action, direction } =
            Tuple.first (bestChoice 3 model)
    in
        { hero | action = action, direction = direction }


type alias Choice =
    { action : Action
    , direction : Direction
    }


noChoice : Choice
noChoice =
    Choice NoAction Down


choices : List Choice
choices =
    [ Choice Build Up
    , Choice Build Right
    , Choice Build Left
    , Choice Build Down
    , Choice Move Up
    , Choice Move Right
    , Choice Move Left
    , Choice Move Down
    ]


bestChoice : Int -> Model -> ( Choice, Float )
bestChoice depth model =
    if depth <= 0 then
        ( noChoice, staticEval model )
    else
        List.foldl (maxEval depth model)
            ( noChoice, 0 )
            (List.filter (isValidChoice model) choices)


maxEval : Int -> Model -> Choice -> ( Choice, Float ) -> ( Choice, Float )
maxEval depth model choice best =
    -- Check if we already have reached the best possible outcome and end early.
    if Tuple.second best >= 1 then
        best
    else
        let
            nextModel =
                nextState choice model

            ( _, value ) =
                bestChoice (depth - 1) nextModel
        in
            if Tuple.second best >= value then
                best
            else
                ( choice, value )


staticEval : Model -> Float
staticEval model =
    if model.gameOver then
        -1
    else
        1


nextState : Choice -> Model -> Model
nextState choice model =
    model
        |> setHeroChoice choice
        |> makeCreepChoices
        |> Update.resolve
        |> Update.execute


setHeroChoice : Choice -> Model -> Model
setHeroChoice { action, direction } model =
    let
        modelHero =
            model.hero
    in
        { model
            | hero = { modelHero | action = action, direction = direction }
        }


makeCreepChoices : Model -> Model
makeCreepChoices model =
    { model | creeps = List.map (Creep.makeChoice model) model.creeps }


isValidChoice : Model -> Choice -> Bool
isValidChoice model { action, direction } =
    if action == Move || action == Build then
        let
            ( dx, dy ) =
                directionToXY direction

            x =
                model.hero.x + dx

            y =
                model.hero.y + dy
        in
            Grid.get model x y == Nothing && Grid.inBounds x y
    else
        True