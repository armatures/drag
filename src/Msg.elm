module Msg exposing (..)

import Id exposing (Id)
import Model exposing (Card)
import Mouse exposing (Coords)


type Msg
    = NoOp
    | MouseUp
    | MouseDown Card
    | MouseMove DragRecord


type alias DragRecord =
    { startId : Id, current : Coords }
