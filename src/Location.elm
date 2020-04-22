module Location exposing (..)

import AssocList as Dict exposing (Dict, get, map)
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


values : LocationStore -> List Location
values (LocationStore locationStore) =
    Dict.values locationStore


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


tableCards : LocationStore -> List ( Id, Coords )
tableCards (LocationStore locationStore) =
    Dict.toList locationStore
        |> List.filterMap
            (\( id, location ) ->
                case location of
                    Table coords ->
                        Just ( id, coords )

                    InHand _ ->
                        Nothing
            )


handCards : LocationStore -> List ( Id, HandPosition )
handCards (LocationStore locationStore) =
    Dict.toList locationStore
        |> List.filterMap
            (\( id, location ) ->
                case location of
                    Table _ ->
                        Nothing

                    InHand i ->
                        Just ( id, i )
            )
        |> List.sortBy Tuple.second


placeCard : Id -> Location -> LocationStore -> LocationStore
placeCard id destination =
    mapLocation id (always (Just destination))
