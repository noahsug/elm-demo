module Game.Hero exposing (create, makeChoice)

import Game.Ai.BuildPlan as BuildPlan
import Game.Ai.Model exposing (..)
import Game.Creep as Creep
import Game.Grid as Grid
import Game.Model exposing (..)
import Game.Movement as Movement
import Game.Structure as Structure
import Game.Update as Update
import Game.Utils
    exposing
        ( directionToXY
        , xyToDirection
        , closestEntity
        , distanceFromCenter
        , facing
        , createEntity
        )


create : Entity
create =
    createEntity


type alias Choice =
    { action : Action
    , direction : Direction
    }


type alias EvaluatedChoice =
    { choice : Choice
    , eval : Float
    }


doNothingChoice : Choice
doNothingChoice =
    Choice NoAction Down


gameOverEval : Float
gameOverEval =
    -1000


maxBuildPlanDepth : Int
maxBuildPlanDepth =
    3


makeChoice : Model -> Entity
makeChoice model =
    let
        bestChoice =
            bestBuildPlanChoice 0 model
    in
        if bestChoice.eval <= gameOverEval then
            doSurvivalActions model
        else
            doChoice model bestChoice.choice


bestBuildPlanChoice : Int -> Model -> EvaluatedChoice
bestBuildPlanChoice depth model =
    List.foldl (maxBuildPlanEval depth model)
        (EvaluatedChoice doNothingChoice gameOverEval)
        (BuildPlan.buildPlans model)


maxBuildPlanEval : Int -> Model -> BuildPlan -> EvaluatedChoice -> EvaluatedChoice
maxBuildPlanEval depth model plan best =
    if best.eval >= plan.eval then
        best
    else
        let
            candidate =
                evalBuildPlan depth plan model
        in
            if candidate.eval > best.eval then
                candidate
            else
                best


evalBuildPlan : Int -> BuildPlan -> Model -> EvaluatedChoice
evalBuildPlan depth plan model =
    -- TODO: Use AStar instead. Problem with this is it doesn't handle when the
    -- hero is directly on the desired square, and it only looks at one possible
    -- path.
    let
        dx =
            plan.x - model.hero.x

        dy =
            plan.y - model.hero.y

        distance =
            abs dx + abs dy
    in
        if model.gameOver then
            EvaluatedChoice doNothingChoice gameOverEval
        else if distance == 1 then
            let
                direction =
                    xyToDirection dx dy

                choice =
                    Choice (Build plan.build) direction
            in
                if choiceIsSafe choice model then
                    EvaluatedChoice choice
                        (plan.eval
                            + futureBuildPlanEval depth model choice
                        )
                else
                    EvaluatedChoice doNothingChoice gameOverEval
        else
            evalBuildPlanMovement depth plan model


evalBuildPlanMovement : Int -> BuildPlan -> Model -> EvaluatedChoice
evalBuildPlanMovement depth plan model =
    let
        dx =
            plan.x - model.hero.x

        dy =
            plan.y - model.hero.y

        xMove =
            Movement.xMovement model model.hero dx

        yMove =
            Movement.yMovement model model.hero dy

        maybeMove =
            if dy /= 0 && yMove.collide == Nothing then
                Just yMove
            else if dx /= 0 && xMove.collide == Nothing then
                Just xMove
            else
                Nothing
    in
        case maybeMove of
            Just move ->
                let
                    choice =
                        Choice (Move 1) move.direction

                    next =
                        model
                            |> nextState choice
                            |> evalBuildPlan depth plan
                in
                    if next.eval <= gameOverEval then
                        EvaluatedChoice doNothingChoice gameOverEval
                    else
                        EvaluatedChoice choice next.eval

            Nothing ->
                EvaluatedChoice doNothingChoice gameOverEval


choiceIsSafe : Choice -> Model -> Bool
choiceIsSafe choice model =
    -- Do nothing twice after doing the choice, if hero survives, choice is
    -- safe.
    model
        |> nextState choice
        |> nextState doNothingChoice
        |> nextState doNothingChoice
        |> not
        << .gameOver


futureBuildPlanEval : Int -> Model -> Choice -> Float
futureBuildPlanEval depth model choice =
    if depth >= maxBuildPlanDepth then
        0
    else
        model
            |> nextState choice
            |> bestBuildPlanChoice (depth + 1)
            |> .eval


doChoice : Model -> Choice -> Entity
doChoice model choice =
    let
        hero =
            model.hero
    in
        { hero | action = choice.action, direction = choice.direction }


doNothing : Model -> Entity
doNothing model =
    let
        hero =
            model.hero
    in
        { hero | action = NoAction }


nextState : Choice -> Model -> Model
nextState choice model =
    { model | hero = doChoice model choice }
        |> makeCreepChoices
        |> makeStructureChoices
        |> Update.resolve
        |> Update.execute



-- Survival Actions


maxDepth : Int
maxDepth =
    3


maxEvalValue : Float
maxEvalValue =
    0


doSurvivalActions : Model -> Entity
doSurvivalActions model =
    let
        hero =
            model.hero

        { action, direction } =
            Tuple.first (bestChoice 0 model)
    in
        { hero | action = action, direction = direction }


choices : List Choice
choices =
    [ Choice (Build Block) Up
    , Choice (Build Block) Right
    , Choice (Build Block) Left
    , Choice (Build Block) Down
    , Choice (Move 1) Up
    , Choice (Move 1) Right
    , Choice (Move 1) Left
    , Choice (Move 1) Down
    ]


bestChoice : Int -> Model -> ( Choice, Float )
bestChoice depth model =
    if depth > maxDepth || model.gameOver then
        ( doNothingChoice, staticEval model depth )
    else
        (List.foldl (maxEval depth model)
            ( doNothingChoice, gameOverValue depth )
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
        stayCentralHeuristic model


stayCentralHeuristic : Model -> Float
stayCentralHeuristic model =
    toFloat -(distanceFromCenter model.hero)


gameOverValue : Int -> Float
gameOverValue depth =
    toFloat (depth - 1000)


makeCreepChoices : Model -> Model
makeCreepChoices model =
    { model | creeps = List.map (Creep.makeChoice model) model.creeps }


makeStructureChoices : Model -> Model
makeStructureChoices model =
    { model | structures = List.map (Structure.makeChoice model) model.structures }


isValidChoice : Model -> Choice -> Bool
isValidChoice model { action, direction } =
    let
        needsEmptySquare =
            case action of
                Move _ ->
                    True

                Build _ ->
                    True

                _ ->
                    False
    in
        if needsEmptySquare then
            let
                ( dx, dy ) =
                    directionToXY direction

                ( x, y ) =
                    ( model.hero.x + dx, model.hero.y + dy )

                usefulMovement =
                    case action of
                        Move _ ->
                            isNearBuildings model x y
                        _ ->
                            True
            in
                Grid.inBounds x y
                    && Grid.get model x y
                    == Nothing
                    && usefulMovement
        else
            True


isNearBuildings : Model -> Int -> Int -> Bool
isNearBuildings model x y =
    case closestEntity model.structures x y of
        Just ( distance, _ ) ->
            distance <= 3

        Nothing ->
            False
