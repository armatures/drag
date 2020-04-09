module Model exposing (..)

import Id exposing (Id)
import Mouse exposing (Coords)


type alias Card =
    { id : Id, location : Location }


type Location
    = Table Coords
    | InHand


mapLocation f card =
    { card | location = f card.location }


mapCoords f card =
    { card | coords = f card.coords }
