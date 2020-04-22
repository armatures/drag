module Location exposing (..)

import AssocList as Dict exposing (Dict, get)
import Id exposing (Id)
import Mouse exposing (Coords, initCoords)


cardLocation : Id -> LocationStore -> Maybe Location
cardLocation cardId (LocationStore locations) =
    get cardId locations


initLocationStore : Int -> LocationStore
initLocationStore count =
    let
        ids =
            List.range 1 count
                |> List.map Id.new

        locations =
            List.range 1 count
                |> List.map
                    (\i ->
                        if modBy 2 i == 0 then
                            Table initCoords

                        else
                            InHand ((i - 1) // 2)
                    )
    in
    List.map2 Tuple.pair ids locations
        |> Dict.fromList
        |> LocationStore


keys : LocationStore -> List Id
keys (LocationStore locationStore) =
    Dict.keys locationStore


mapLocation : Id -> (Maybe Location -> Maybe Location) -> LocationStore -> LocationStore
mapLocation id f (LocationStore locationStore) =
    LocationStore <| Dict.update id f locationStore


type LocationStore
    = LocationStore (Dict Id Location)


type Location
    = Table Coords
    | InHand HandPosition


type alias HandPosition =
    Int


tableCards : List { card | id : Id } -> LocationStore -> List ( { card | id : Id }, Coords )
tableCards cards locationStore =
    List.filterMap
        (\card ->
            case cardLocation card.id locationStore of
                Nothing ->
                    Nothing

                Just (Table coords) ->
                    Just ( card, coords )

                Just (InHand _) ->
                    Nothing
        )
        cards


handCards : List { card | id : Id } -> LocationStore -> List ( { card | id : Id }, HandPosition )
handCards cards locationStore =
    List.filterMap
        (\card ->
            case cardLocation card.id locationStore of
                Nothing ->
                    Nothing

                Just (Table _) ->
                    Nothing

                Just (InHand i) ->
                    Just ( card, i )
        )
        cards
