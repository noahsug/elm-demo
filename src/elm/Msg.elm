module Msg exposing (..)

import Time
import Window


type Msg
    = Tick Time.Time
    | Resize Window.Size
