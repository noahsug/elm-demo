module Game.Creep exposing (create, makeChoice)

import Game.Model exposing (..)
import Game.Grid as Grid
import Utils exposing (isNothing, sign)
import Game.Utils exposing (xyToDirection, facing)


create : Entity
create =
    { action = Move
    , direction = Right
    , x = -6
    , y = 0
    , kind = Creep
    }


makeChoice : Model -> Entity -> Entity
makeChoice model creep =
    let
        dx =
            model.hero.x - creep.x

        dy =
            model.hero.y - creep.y

        moveX =
            (abs dx) > (abs dy)

        desiredX =
            creep.x + sign dx

        desiredY =
            creep.y + sign dy

        collideX =
            Grid.get model desiredX creep.y

        collideY =
            Grid.get model creep.x desiredY
    in
        if isKind Hero collideX then
            { creep | action = KillHero, direction = xyToDirection dx 0 }
        else if isKind Hero collideY then
            { creep | action = KillHero, direction = xyToDirection 0 dy }
        else if canKillHeroWithoutMoving model creep then
            { creep | action = NoAction }
        else if moveX && collideX == Nothing then
            { creep | action = Move, direction = xyToDirection dx 0 }
        else if not moveX && collideY == Nothing then
            { creep | action = Move, direction = xyToDirection 0 dy }
        else if dx /= 0 && collideX == Nothing then
            { creep | action = Move, direction = xyToDirection dx 0 }
        else if dy /= 0 && collideY == Nothing then
            { creep | action = Move, direction = xyToDirection 0 dy }
        else if moveX && isKind Block collideX then
            { creep | action = Attack, direction = xyToDirection dx 0 }
        else if isKind Block collideY then
            { creep | action = Attack, direction = xyToDirection 0 dy }
        else
            { creep | action = NoAction }


canKillHeroWithoutMoving : Model -> Entity -> Bool
canKillHeroWithoutMoving model creep =
    case model.hero.action of
        Move ->
            let
                (heroX, heroY) = facing model.hero

                dx = heroX - creep.x

                dy = heroY - creep.y
            in
                if (abs dx) + (abs dy) == 1 then
                      True
                else
                    False
        _ ->
            False

isKind : EntityType -> Maybe Entity -> Bool
isKind kind maybeEntity =
    case maybeEntity of
        Just entity ->
            entity.kind == kind

        Nothing ->
            False
