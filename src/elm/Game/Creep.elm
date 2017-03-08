module Game.Creep exposing (create, act)

import Game.Model exposing (..)

create : Entity
create =
    { action = MOVE
    , direction = RIGHT
    , x = -15
    , y = 0
    }


act : Model -> Entity -> Entity
act model creep =
    creep

-- moveCreeps : Float -> Model -> Model
-- moveCreeps dt model =
--     let
--         rotation =
--             Config.rotations * (2 * pi * dt)
--     in
--         { model
--             | creeps =
--                 model.creeps
--                     |> List.map
--                         (Entity.move (Config.speed * dt)
--                             << Entity.rotateTowards
--                                 rotation
--                                 model.hero.x
--                                 model.hero.y
--                         )
--         }
