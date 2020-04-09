module Model exposing (..)

import Id exposing (Id)
import Mouse exposing (Coords)


type alias Card =
    { id : Id, location : Location }


type Location
    = Table Coords
    | InHand


cardsInHand : List Card -> List Card
cardsInHand =
    List.filter isInHand


isInHand : Card -> Bool
isInHand card =
    case card.location of
        Table _ ->
            False

        InHand ->
            True


isOnTable : Card -> Bool
isOnTable card =
    case card.location of
        Table _ ->
            True

        InHand ->
            False


tableCards : List Card -> List Card
tableCards =
    List.filter isOnTable


mapLocation f card =
    { card | location = f card.location }


mapCoords f card =
    { card | coords = f card.coords }
