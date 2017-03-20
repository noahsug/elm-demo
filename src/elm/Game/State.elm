module Game.State exposing (init, update)

import Game.Creep as Creep
import Game.Hero as Hero
import Game.Model exposing (..)
import Game.Update as Update


init : Model
init =
    { hero = Hero.create
    , creeps = [ Creep.create, Creep.create2 ]
    , structures = []
    , gameOver = False
    }


update : Model -> Model
update model =
    model
        |> Update.execute
        |> makeHeroChoice
        |> makeCreepChoices
        |> Update.resolve


makeHeroChoice : Model -> Model
makeHeroChoice model =
    let
        modelHero =
            model.hero

        hero =
            if model.gameOver then
                { modelHero | action = NoAction }
            else
                Hero.makeChoice model
    in
        { model | hero = hero }


makeCreepChoices : Model -> Model
makeCreepChoices model =
    { model | creeps = List.map (Creep.makeChoice model) model.creeps }
