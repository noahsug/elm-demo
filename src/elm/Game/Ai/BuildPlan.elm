module Game.Ai.BuildPlan exposing (buildPlans)

import Config
import Game.Ai.Model exposing (..)
import Game.Grid as Grid
import Game.Model exposing (..)


buildPlans : Model -> List BuildPlan
buildPlans model =
    initEval
        |> removeNonemptyTiles model
        -- TODO: do a bunch of other evals to see what would help board state
        |> List.filter (\buildPlan -> buildPlan.eval > 0)
        |> evalHeroPosition model
        |> List.sortBy .eval


initEval : List BuildPlan
initEval =
    -- at ideal position: 6
    -- 1 away from ideal: 5
    -- 2 away from ideal: 2
    -- otherwise: negative
    List.map
        (\( x, y ) ->
            let
                idealBlockDistance =
                    Config.heroRadius // 3

                idealTurretDistance = idealBlockDistance - 1

                blockDistanceSquared =
                    abs
                        (idealBlockDistance
                            * idealBlockDistance
                            - (x * x + y * y)
                        )

                turretDistanceSquared =
                    abs
                        (idealTurretDistance
                            * idealTurretDistance
                            - (x * x + y * y)
                        )

                blockEval =
                    toFloat (6 - blockDistanceSquared)

                turretEval =
                    toFloat (6 - turretDistanceSquared)

                structureType = if turretEval >= blockEval then Turret else Block
            in
                { x = x
                , y = y
                , build = structureType
                , eval = max turretEval blockEval
                }
        )
        Grid.positions


removeNonemptyTiles : Model -> List BuildPlan -> List BuildPlan
removeNonemptyTiles model buildPlans =
    List.filter
        (\buildPlan ->
            case Grid.get model buildPlan.x buildPlan.y of
                Nothing ->
                    True

                _ ->
                    False
        )
        buildPlans


evalHeroPosition : Model -> List BuildPlan -> List BuildPlan
evalHeroPosition model buildPlans =
    -- 0.5 for each square hero has to move to build the building.
    List.map
        (\buildPlan ->
            let
                heroDistance =
                    abs
                        (1
                            - (abs (model.hero.x - buildPlan.x)
                                + abs (model.hero.y - buildPlan.y)
                              )
                        )

                heroEval =
                    toFloat -heroDistance
            in
                { buildPlan | eval = buildPlan.eval + heroEval * 0.5 }
        )
        buildPlans
