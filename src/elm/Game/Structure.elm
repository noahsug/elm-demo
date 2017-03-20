module Game.Structure exposing (createBlock, createTurret)

import Game.Model exposing (..)

createBlock : Entity
createBlock =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , px = 0
    , py = 0
    , kind = Structure Block
    , health = 3
    }


createTurret : Entity
createTurret =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , px = 0
    , py = 0
    , kind = Structure Turret
    , health = 1
    }
