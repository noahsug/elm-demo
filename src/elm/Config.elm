module Config exposing (..)

-- Controls zoom level.
screenHeight : Int
screenHeight =
    711


-- Size of single tile in the game grid.
gridSize : Float
gridSize =
    24


-- Radius area hero can travel and where creeps cannot be spawned, in grid
-- units.
heroRadius : Int
heroRadius =
    8


-- Seconds until the game logic updates.
gameUpdateTime : Float
gameUpdateTime =
    0.5


ticksUntilGameOver : Int
ticksUntilGameOver =
    100


dashDistance : Int
dashDistance =
    3

dashDuration : Int
dashDuration =
    3


-- Not Used

creepReadyRate : Int
creepReadyRate =
    0


startingCreeps : Int
startingCreeps =
    0
