module Game.Hero exposing (create, act)

import Game.Model exposing (..)


create : Entity
create =
    { action = NONE
    , direction = DOWN
    , x = 0
    , y = 0
    }


act : Model -> Entity
act model =
    let
        hero =
            model.hero

        ( action, direction ) =
            case (hero.action, hero.direction) of
                (MOVE, UP) ->
                    ( BUILD, LEFT )

                (BUILD, LEFT) ->
                    ( MOVE, RIGHT )

                _ ->
                    ( MOVE, UP )
    in
        { hero | action = action, direction = direction }



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
