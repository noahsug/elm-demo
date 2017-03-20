module Helpers exposing (..)

import Game.Model exposing (..)
import Game.State
import Game.Structure as Structure
import Game.Creep as Creep
import Game.Hero as Hero

initModel =
    Game.State.init


createHero x y action direction =
    let
        hero =
            Hero.create
    in
        { hero | x = x, y = y, action = action, direction = direction }


createCreep x y action direction =
    let
        creep =
            Creep.create
    in
        { creep | x = x, y = y, action = action, direction = direction }


createBlock : Int -> Int -> Entity
createBlock x y =
    let
        block =
            Structure.createBlock
    in
        { block | x = x, y = y }
