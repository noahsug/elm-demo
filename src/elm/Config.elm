module Config exposing (..)

-- Controls zoom level.
screenSurfaceArea : Int
screenSurfaceArea =
  400 * 711


-- Size of single block in the game grid.
gridSize : Float
gridSize =
    15

-- Seconds until the game logic updates.
gameUpdateTime : Float
gameUpdateTime =
  0.35

-- speed : Float
-- speed =
--     100
--
--
-- rotations : Float
-- rotations =
--     0.25
--
--
-- creepSpawnX : Float
-- creepSpawnX =
--     -250
--
--
-- creepSpawnY : Float
-- creepSpawnY =
--     0
--
--
-- creepSpawnRate : Float
-- creepSpawnRate =
--     3
