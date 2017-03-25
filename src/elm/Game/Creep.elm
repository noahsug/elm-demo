module Game.Creep exposing (create, create2, makeChoice)

import Game.Model exposing (..)
import Game.Utils exposing (xyToDirection, facing, position, isStructure, isTurret)
import Game.Movement exposing (..)
import Maybe.Extra


create : Entity
create =
    { action = Move
    , direction = Right
    , x = -6
    , y = 0
    , px = -6
    , py = 0
    , kind = Creep
    , health = 4
    }


create2 : Entity
create2 =
    { action = Move
    , direction = Right
    , x = 6
    , y = 6
    , px = 6
    , py = 6
    , kind = Creep
    , health = 1
    }


makeChoice : Model -> Entity -> Entity
makeChoice model creep =
    let
        dx =
            model.hero.x - creep.x

        dy =
            model.hero.y - creep.y

        distance =
            abs dx + abs dy
    in
        if distance == 1 then
            { creep | action = KillHero, direction = xyToDirection dx dy }
        else if distance == 2 && canKillHeroWithoutMoving model creep then
            { creep | action = NoAction }
        else if abs dx == abs dy then
            doDiagonalMovement model creep
        else
            doNormalMovement model creep


canKillHeroWithoutMoving : Model -> Entity -> Bool
canKillHeroWithoutMoving model creep =
    case model.hero.action of
        Move ->
            let
                ( nextHeroX, nextHeroY ) =
                    facing model.hero

                dx =
                    nextHeroX - creep.x

                dy =
                    nextHeroY - creep.y

                distance =
                    abs dx + abs dy
            in
                if distance == 1 then
                    True
                else
                    False

        _ ->
            False



-- Try to avoid structures.


doDiagonalMovement : Model -> Entity -> Entity
doDiagonalMovement model creep =
    let
        dx =
            model.hero.x - creep.x

        dy =
            model.hero.y - creep.y

        xMove =
            xMovement model creep dx

        yMove =
            yMovement model creep dy
    in
        doBestMovement yMove xMove creep



-- Move in primary direction if possible, even if there's a structure.


doNormalMovement : Model -> Entity -> Entity
doNormalMovement model creep =
    let
        dx =
            model.hero.x - creep.x

        dy =
            model.hero.y - creep.y

        primaryMovement =
            if abs dx > abs dy then
                xMovement model creep dx
            else
                yMovement model creep dy
    in
        if isValidMovement creep primaryMovement then
            doMovement primaryMovement creep
        else
            doSecondaryMovement model creep


doSecondaryMovement : Model -> Entity -> Entity
doSecondaryMovement model creep =
    let
        dx =
            model.hero.x - creep.x

        dy =
            model.hero.y - creep.y
    in
        if abs dx > abs dy then
            if dy == 0 then
                -- There are two possible secondary y movements.
                doBestMovement (yMovement model creep 1)
                    (yMovement model creep -1)
                    creep
            else
                doMovementOrNothing (yMovement model creep dy) creep
        else if dx == 0 then
            -- There are two possible secondary x movements.
            doBestMovement (xMovement model creep 1)
                (xMovement model creep -1)
                creep
        else
            doMovementOrNothing (xMovement model creep dx) creep


isValidMovement : Entity -> Movement -> Bool
isValidMovement creep move =
    if
        creep.x
            + move.x
            == creep.px
            && creep.y
            + move.y
            == creep.py
    then
        False
    else
        case move.collide of
            Just entity ->
                entity.kind /= Creep

            Nothing ->
                True


doBestMovement : Movement -> Movement -> Entity -> Entity
doBestMovement move1 move2 creep =
    let
        move1Invalid =
            not (isValidMovement creep move1)

        move2Invalid =
            not (isValidMovement creep move2)
    in
        if move2Invalid && move2Invalid then
            { creep | action = NoAction }
        else if move2Invalid then
            doMovement move1 creep
        else if move1Invalid then
            doMovement move2 creep
        else if collidesWith isTurret move1 then
            doMovement move1 creep
        else if collidesWith isTurret move2 then
            doMovement move2 creep
        else if move1.collide == Nothing then
            doMovement move1 creep
        else
            doMovement move2 creep


collidesWith : (Entity -> Bool) -> Movement -> Bool
collidesWith isEntityFn move =
    Maybe.Extra.unwrap False isEntityFn move.collide


doMovementOrNothing : Movement -> Entity -> Entity
doMovementOrNothing move creep =
    if isValidMovement creep move then
        doMovement move creep
    else
        { creep | action = NoAction }


doMovement : Movement -> Entity -> Entity
doMovement move creep =
    let
        action =
            case move.collide of
                Just entity ->
                    case entity.kind of
                        Structure _ ->
                            Attack entity

                        Hero ->
                            KillHero

                        Creep ->
                            NoAction

                Nothing ->
                    Move
    in
        { creep | action = action, direction = move.direction }
