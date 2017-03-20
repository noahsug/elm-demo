module Game.Grid exposing (..)

import Config
import Game.Model exposing (..)
import List.Extra


positions : List ( Int, Int )
positions =
    List.range 0 (4 * Config.heroRadius * Config.heroRadius - 1)
        |> List.map
            (\i ->
                let
                    dx =
                        i // Config.heroRadius

                    dy =
                        i % Config.heroRadius

                    x =
                        dx - Config.heroRadius

                    y =
                        dy - Config.heroRadius
                in
                    ( x, y )
            )
        |> List.filter (uncurry inBounds)


get : Model -> Int -> Int -> Maybe Entity
get model x y =
    List.Extra.find (\e -> e.x == x && e.y == y)
        (model.hero :: (model.creeps ++ model.structures))


inBounds : Int -> Int -> Bool
inBounds x y =
    x * x + y * y <= Config.heroRadius * Config.heroRadius
