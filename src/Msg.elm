module Msg exposing (..)

import Model exposing (Card)
import Mouse exposing (Coords)


type Msg
    = NoOp
    | MouseUp
    | MouseDown Card
    | MouseMove DragRecord


type alias DragRecord =
    { start : Card, current : Coords }
