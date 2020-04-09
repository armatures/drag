module Model exposing (..)

import Id exposing (Id)
import Mouse exposing (Coords)


type alias Card =
    { id : Id, coords : Coords }


mapCoords f card =
    { card | coords = f card.coords }
