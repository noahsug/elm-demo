module Game.Creep exposing (create, act)

import Game.Model exposing (..)
import Game.Grid as Grid
import Utils exposing (isNothing, sign)
import Game.Utils exposing (xyToDirection)


create : Entity
create =
    { action = Move
    , direction = Right
    , x = -6
    , y = 0
    , kind = Creep
    }


act : Model -> Entity -> Entity
act model creep =
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
        else if isKind Hero collideX then
            { creep | action = KillHero, direction = xyToDirection 0 dy }
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
        else if moveX && isKind Block collideY then
            { creep | action = Attack, direction = xyToDirection 0 dy }
        else
            { creep | action = NoAction }


isKind : EntityType -> Maybe Entity -> Bool
isKind kind maybeEntity =
    case maybeEntity of
        Just entity ->
            entity.kind == kind

        Nothing ->
            False
