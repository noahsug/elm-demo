module Game.Hero exposing (create, act)

import Game.Model exposing (..)


create : Entity
create =
    { action = NoAction
    , direction = Down
    , x = 0
    , y = 0
    , kind = Hero
    }


act : Model -> Entity
act model =
    let
        hero =
            model.hero

        ( action, direction ) =
            case ( hero.action, hero.direction ) of
                ( Move, Up ) ->
                    ( Build, Left )

                ( Build, Left ) ->
                    ( Move, Right )

                _ ->
                    ( Move, Up )
    in
        { hero | action = action, direction = direction }
