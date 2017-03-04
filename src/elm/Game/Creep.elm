module Creep exposing (..)

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
