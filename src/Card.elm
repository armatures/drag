module Card exposing (..)

import AssocList as Dict exposing (Dict)
import Id exposing (Id)
import Mouse exposing (Coords, initCoords)


type alias Card =
    { id : Id, coords : Coords }


mapCoords f card =
    { card | coords = f card.coords }


initCards : Int -> Dict Id Card
initCards count =
    let
        ids =
            List.range 1 count |> List.map Id.new
    in
    List.map2 Card
        ids
        (List.repeat count initCoords)
        |> List.map2 Tuple.pair ids
        |> Dict.fromList
