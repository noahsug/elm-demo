module Game.Hero exposing (create, makeChoice)

import Game.Creep as Creep
import Game.Grid as Grid
import Game.Model exposing (..)
import Game.Update as Update
import Game.Utils exposing (directionToXY, distanceFromCenter)
import List.Extra


create : Entity
create =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , px = 0
    , py = 0
    , kind = Hero
    , health = 1
    }


maxDepth : Int
maxDepth =
    4


maxEvalValue : Float
maxEvalValue =
    100


makeChoice : Model -> Entity
makeChoice model =
    let
        hero =
            model.hero

        { action, direction } =
            Tuple.first (bestChoice 0 model)
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
    if depth > maxDepth || model.gameOver then
        ( noChoice, staticEval model depth )
    else
        (List.foldl (maxEval depth model)
            ( noChoice, gameOverValue )
            (List.filter (isValidChoice model) choices)
        )


maxEval : Int -> Model -> Choice -> ( Choice, Float ) -> ( Choice, Float )
maxEval depth model choice best =
    -- Check if we already have reached the best possible outcome and end early.
    if Tuple.second best >= maxEvalValue then
        best
    else
        let
            nextModel =
                nextState choice model

            ( _, value ) =
                bestChoice (depth + 1) nextModel
        in
            if Tuple.second best >= value then
                best
            else
                ( choice, value )


staticEval : Model -> Int -> Float
staticEval model depth =
    if model.gameOver then
        gameOverValue depth
    else
        0.5 * stayCentralHeuristic model + buildBlocksHeuristic model

gameOverValue : Int -> Float
gameOverValue depth =
    toFloat (depth - 1000)

stayCentralHeuristic : Model -> Float
stayCentralHeuristic model =
    toFloat -(distanceFromCenter model.hero)


buildBlocksHeuristic : Model -> Float
buildBlocksHeuristic model =
    List.foldl
        (\block result ->
            let
                distance =
                    abs (4 - distanceFromCenter block)
            in
                result
                    + if distance == 0 then
                        5
                      else if distance < 1 then
                        3
                      else
                        1
        )
        0
        model.blocks



--closestEntityStaticEval : Model -> Float
--closestEntityStaticEval model =
--    let
--        ( closest, distance ) =
--            closestEntity model
--    in
--        case closest.kind of
--            Creep ->
--                if distance <= 2 then
--                    -100
--                else if distance <= 4 then
--                    -10
--                else
--                    -1
--
--            Block ->
--                if distance <= 4 then
--                    1
--                else
--                    0
--
--            _ ->
--                0
--
--
--closestEntity : Model -> ( Entity, Int )
--closestEntity model =
--    List.foldl
--        (\entity closest ->
--            let
--                distance =
--                    Game.Utils.distance model.hero entity
--            in
--                if distance < Tuple.second closest then
--                    ( entity, distance )
--                else
--                    closest
--        )
--        ( model.hero, -1000000 )
--        -- We check creeps first because we want them to break ties.
--        (model.creeps ++ model.blocks)


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
            Grid.get model x y
                == Nothing
                && (distanceFromCenter model.hero)
                <= 7
    else
        True
