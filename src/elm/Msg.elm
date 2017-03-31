module Msg exposing (..)

import Time
import Window
import Input

type Msg
    = Tick Time.Time
    | Resize Window.Size
    | Click Input.Position
