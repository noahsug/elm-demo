module Game.Block exposing (create)

import Game.Model exposing (..)

create : Entity
create =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , px = 0
    , py = 0
    , kind = Block
    , health = 3
    }
