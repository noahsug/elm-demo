module Game.Levels exposing (levels)

import Game.Hero as Hero
import Array as Array exposing (Array)


levels =
    Array.fromList
        [ { hero = Hero.create
          , creeps = []
          , structures = []
          , creepLine =
                [ Creep.create Tank
                , Creep.create Tank
                , Creep.create Tank
                , Creep.create Tank
                , Creep.create Tank
                , Creep.create Tank
                ]
          }
        ]
