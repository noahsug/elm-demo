module Input exposing (Position, onClick)

import Html
import Html.Events exposing (on)
import Json.Decode as Decode exposing (field)


type alias Position =
    { x : Int, y : Int }


mouseEventDecoder : Decode.Decoder Position
mouseEventDecoder =
    Decode.map2
        Position
        (field "clientX" Decode.int)
        (field "clientY" Decode.int)


onClick : (Position -> msg) -> Html.Attribute msg
onClick target =
    on "click" (Decode.map target mouseEventDecoder)
