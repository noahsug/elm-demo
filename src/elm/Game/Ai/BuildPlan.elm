module Game.Ai.BuildPlan exposing (buildPlans)

import Config
import Game.Ai.Config as Config
import Game.Ai.Model exposing (..)
import Game.Grid as Grid
import Game.Model exposing (..)
import Game.Utils exposing (isTurret, isBlock, isStructure, distanceFromEntity)
import Utils exposing (normalize, (??))


buildLocations : List ( Int, Int )
buildLocations =
    Grid.positions
        |> List.filter
            (\( x, y ) ->
                let
                    r =
                        Config.heroRadius - 1
                in
                    -- Don't build on the edge of the map.
                    x * x + y * y < r * r
            )


buildPlans : Model -> List BuildPlan
buildPlans model =
    Grid.positions
        |> List.filter (\( x, y ) -> Grid.get model x y == Nothing)
        |> List.map
            (\( x, y ) ->
                let
                    turret =
                        turretEval model x y

                    block =
                        blockEval model x y

                    build =
                        max turret block

                    hero =
                        heroEval model x y
                in
                    { x = x
                    , y =
                        y
                        --, d = distanceEval 3 x y
                        --, t = protectTurretEval model x y
                        --, c = blockCreepEval model x y
                        --, h = dontBlockHeroEval model x y
                    , build =
                        if turret >= block then
                            Turret
                        else
                            Block
                    , eval =
                        if build <= 0 then
                            -1000
                        else
                            hero + build
                    }
            )
        |> List.filter (\buildPlan -> buildPlan.eval > -1000)
        |> List.sortBy (\buildPlan -> -buildPlan.eval)


turretEval : Model -> Int -> Int -> Float
turretEval model x y =
    Config.buildTurret
        + distanceEval Config.turretDistance x y
        + protectedByBlockEval model x y
        + dontBlockHeroEval model x y


protectedByBlockEval : Model -> Int -> Int -> Float
protectedByBlockEval model x y =
    if x == 0 && y == 0 then
        0
    else
        let
            xProtected =
                if isBlock ?? (Grid.get model (x + normalize x) y) then
                    toFloat (abs x) / toFloat (abs x + abs y)
                else
                    0

            yProtected =
                if isBlock ?? (Grid.get model x (y + normalize y)) then
                    toFloat (abs y) / toFloat (abs x + abs y)
                else
                    0
        in
            if xProtected == 0 && yProtected == 0 then
                0
            else if x /= 0 && y /= 0 && (xProtected == 0 || yProtected == 0) then
                (xProtected + yProtected) * Config.buildHalfProtectedTurret
            else
                (xProtected + yProtected) * Config.buildFullyProtectedTurret


blockEval : Model -> Int -> Int -> Float
blockEval model x y =
    Config.buildBlock
        + (distanceEval Config.blockDistance x y)
        * Config.blockDistanceMultiplier
        + protectTurretEval model x y
        + blockCreepEval model x y
        + dontBlockHeroEval model x y


protectTurretEval : Model -> Int -> Int -> Float
protectTurretEval model x y =
    if x == 0 && y == 0 then
        0
    else
        let
            xProtected =
                if isTurret ?? (Grid.get model (x - normalize x) y) then
                    toFloat (abs x) / toFloat (abs x + abs y)
                else
                    0

            yProtected =
                if isTurret ?? (Grid.get model x (y - normalize y)) then
                    toFloat (abs y) / toFloat (abs x + abs y)
                else
                    0
        in
            if xProtected == 0 && yProtected == 0 then
                0
            else
                (xProtected + yProtected) * Config.protectTurret


dontBlockHeroEval : Model -> Int -> Int -> Float
dontBlockHeroEval model x y =
    if not (heroCanMove model x y) then
        -100
    else if canWalkToCenter model x y then
        Config.dontBlockHero
    else
        0


heroCanMove : Model -> Int -> Int -> Bool
heroCanMove model x y =
    let
        up =
            Grid.get model model.hero.x (model.hero.y + 1)

        down =
            Grid.get model model.hero.x (model.hero.y - 1)

        right =
            Grid.get model (model.hero.x + 1) model.hero.y

        left =
            Grid.get model (model.hero.x - 1) model.hero.y
    in
        if
            up
                == Nothing
                || down
                == Nothing
                || left
                == Nothing
                || right
                == Nothing
        then
            True
        else
            False



--    let
--        x1 = min 0 model.hero.x
--        y1 = min 0 model.hero.y
--        x2 = max 0 model.hero.x
--        y2 = max 0 model.hero.y
--    in
--        if x >= x1 && x <= x2 && y >= y1 && y <= y2 then
--            -4
--        else
--            0


canWalkToCenter : Model -> Int -> Int -> Bool
canWalkToCenter model x y =
    walkToCenter model x y model.hero.x model.hero.y /= Nothing


walkToCenter : Model -> Int -> Int -> Int -> Int -> Maybe ( Int, Int )
walkToCenter model blockedX blockedY x y =
    let
        newX =
            x - normalize x

        newY =
            y - normalize y

        collideX =
            (newX == blockedX && y == blockedY)
                || (isStructure ?? Grid.get model newX y)

        collideY =
            (x == blockedX && newY == blockedY)
                || (isStructure ?? Grid.get model x newY)
    in
        if x == 0 && y == 0 then
            Just ( 0, 0 )
        else if newY /= y && not collideY then
            walkToCenter model blockedX blockedY x newY
        else if newX /= x && not collideX then
            walkToCenter model blockedX blockedY newX y
        else
            Nothing


blockCreepEval : Model -> Int -> Int -> Float
blockCreepEval model x y =
    let
        ( _, creepX, creepY ) =
            closestCreep model

        x1 =
            min creepX model.hero.x

        y1 =
            min creepY model.hero.y

        x2 =
            max creepX model.hero.x

        y2 =
            max creepY model.hero.y

        --distance = abs (creepX - model.hero.x) +
        --           abs (creepY - model.hero.y)
    in
        if x >= x1 && x <= x2 && y >= y1 && y <= y2 then
            Config.blockCreep
        else
            0


closestCreep : Model -> ( Int, Int, Int )
closestCreep model =
    List.foldl
        (\creep ( bestDistance, x, y ) ->
            let
                distance =
                    distanceFromEntity model.hero creep
            in
                if distance < bestDistance then
                    ( distance, creep.x, creep.y )
                else
                    ( bestDistance, x, y )
        )
        ( 1000, 0, 10 )
        model.creeps


distanceEval : Int -> Int -> Int -> Float
distanceEval ideal x y =
    if (abs x == ideal || abs y == ideal) && abs x + abs y < ideal * 2 then
        Config.buildAtDistance
    else
        0



--case abs (ideal - abs x - abs y) of
--    0 ->
--        20
--
--    1 ->
--        1
--
--    2 ->
--        0
--
--    _ ->
--        -1


heroEval : Model -> Int -> Int -> Float
heroEval model x y =
    heroDistanceEval model x y


heroDistanceEval : Model -> Int -> Int -> Float
heroDistanceEval model x y =
    -- 0.95 for each square hero has to move to build the building.
    let
        distance =
            abs (model.hero.x - x)
                + abs (model.hero.y - y)
                - 1
    in
        toFloat -distance * Config.buildNearHero
