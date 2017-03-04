module Game.State exposing (init, update)

import Game.Model as Model exposing (Model)


init : Model
init =
    {}


update : Model -> Model
update model =
  model
--     let
--         (creeps, ticksUntilSpawn) = Spawn.maybeSpawnCreep model
--     in
--
--         { model
--             | tick = model.tick + 1
--             , ticksUntilSpawn = ticksUntilSpawn
--             , creeps = List.map (Creep.move model) creeps
--             , hero = Hero.move model }
