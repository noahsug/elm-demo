module Hero exposing (..)


-- moveHero : Float -> Model -> Model
-- moveHero dt model =
--     { model
--         | hero =
--             model.hero
--                 |> rotateHero dt
--                 |> Entity.move (Config.speed * dt)
--     }
--
--
--
--
--
-- rotateHero : Float -> Entity.Model -> Entity.Model
-- rotateHero dt entity =
--     let
--         maxRotation =
--             Config.rotations * (2 * pi * dt)
--
--         newRotation =
--             entity.rotation + maxRotation
--     in
--         { entity | rotation = newRotation }
