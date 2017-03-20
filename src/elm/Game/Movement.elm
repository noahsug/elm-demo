module Game.Movement exposing (..)

import Game.Grid as Grid
import Game.Model exposing (..)

xMovement : Model -> Entity -> Int -> Movement
xMovement model entity dx =
    if dx > 0 then
        { x = 1
        , y = 0
        , direction = Right
        , collide = Grid.get model (entity.x + 1) entity.y
        }
    else
        { x = -1
        , y = 0
        , direction = Left
        , collide = Grid.get model (entity.x - 1) entity.y
        }


yMovement : Model -> Entity -> Int -> Movement
yMovement model entity dy =
    if dy > 0 then
        { x = 0
        , y = 1
        , direction = Up
        , collide = Grid.get model entity.x (entity.y + 1)
        }
    else
        { x = 0
        , y = -1
        , direction = Down
        , collide = Grid.get model entity.x (entity.y - 1)
        }
