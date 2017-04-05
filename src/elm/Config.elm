module Config exposing (..)

-- Controls zoom level.
screenSurfaceArea : Int
screenSurfaceArea =
    400 * 711


-- Size of single tile in the game grid.
gridSize : Float
gridSize =
    15


-- Radius area hero can travel and where creeps cannot be spawned, in grid
-- units.
heroRadius : Int
heroRadius =
    10


-- Seconds until the game logic updates.
gameUpdateTime : Float
gameUpdateTime =
    0.5


ticksUntilGameOver : Int
ticksUntilGameOver =
    100


creepReadyRate : Int
creepReadyRate =
    0


startingCreeps : Int
startingCreeps =
    3
