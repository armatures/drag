module Location exposing (HandPosition, Location(..), LocationStore, cardLocation, handCards, initLocationStore, keys, mapLocation, placeCard, tableCards, toList_test, values)

import AssocList as Dict exposing (Dict, get)
import Id exposing (Id, toInt_test)
import Mouse exposing (Coords, initCoords)


cardLocation : Id -> LocationStore -> Maybe Location
cardLocation cardId (LocationStore locations) =
    get cardId locations


toList_test : LocationStore -> List ( Int, Location )
toList_test (LocationStore locationStore) =
    Dict.toList locationStore
        |> List.map (Tuple.mapFirst toInt_test)
        |> List.sortBy Tuple.first


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
mapLocation id f =
    mapDict (Dict.update id f)


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


mapDict : (Dict Id Location -> Dict Id Location) -> LocationStore -> LocationStore
mapDict f (LocationStore locationStore) =
    LocationStore (f locationStore)


placeCard : Id -> Location -> LocationStore -> LocationStore
placeCard id destination locationStore =
    case destination of
        InHand i ->
            mapDict
                (Dict.map
                    (\id_ loc ->
                        if id == id_ then
                            destination

                        else
                            case loc of
                                Table _ ->
                                    loc

                                InHand oldHandIndex ->
                                    if oldHandIndex >= i then
                                        InHand (oldHandIndex + 1)

                                    else
                                        loc
                    )
                )
                locationStore

        Table _ ->
            mapLocation id (always (Just destination)) locationStore
