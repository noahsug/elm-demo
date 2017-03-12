module Game.Grid exposing (get)

import Game.Model exposing (..)
import List.Extra


get : Model -> Int -> Int -> Maybe Entity
get model x y =
    List.Extra.find (\e -> e.x == x && e.y == y)
        (model.hero :: (model.creeps ++ model.blocks))
