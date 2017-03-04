module View exposing (view)

import Collage
import Color
import Element
import Entity
import Html
import Model exposing (..)
import Msg exposing (..)
import Screen


view : Model -> Html.Html Msg
view model =
    Element.toHtml
        (Collage.collage
            (Screen.actualWidth model.screen)
            (Screen.actualHeight model.screen)
            (drawHero model :: List.map (drawCreep model) model.creeps)
        )


drawHero : Model -> Collage.Form
drawHero model =
    Collage.circle (Screen.toActual model.screen 15)
        |> Collage.filled Color.black
        |> Collage.move
            ( Screen.toActual model.screen model.hero.x
            , Screen.toActual model.screen model.hero.y
            )
        |> Collage.rotate (degrees 45)


drawCreep : Model -> Entity.Model -> Collage.Form
drawCreep model entity =
    Collage.circle (Screen.toActual model.screen 15)
        |> Collage.filled Color.red
        |> Collage.move
            ( Screen.toActual model.screen entity.x
            , Screen.toActual model.screen entity.y
            )
