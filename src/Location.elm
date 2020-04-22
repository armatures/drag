module Location exposing (HandPosition, Location(..), LocationStore, cardLocation, handCards, initLocationStore, keys, placeCard, setLocation, tableCards, toList_test, values)

import Id exposing (Id, show, toInt_test)
import List exposing (filterMap)
import List.Extra
import Mouse exposing (Coords, initCoords)


findHandCard : Id -> LocationStore -> Maybe Location
findHandCard cardId (LocationStore locations) =
    List.Extra.findIndex ((==) cardId) locations.hand
        |> Maybe.map InHand


findTableCard : Id -> LocationStore -> Maybe Location
findTableCard cardId (LocationStore locations) =
    List.Extra.find (Tuple.first >> (==) cardId) locations.table
        |> Maybe.map (Tuple.second >> Table)


cardLocation : Id -> LocationStore -> Maybe Location
cardLocation cardId locations =
    filterMap identity
        [ findHandCard cardId locations
        , findTableCard cardId locations
        ]
        |> List.head


toList_test : LocationStore -> List ( Int, Location )
toList_test =
    toList
        >> List.map (Tuple.mapFirst toInt_test)
        >> List.sortBy Tuple.first


initLocationStore : Int -> LocationStore
initLocationStore count =
    List.range 1 count
        |> List.foldr
            (\i acc ->
                if modBy 2 i == 0 then
                    mapTable ((::) ( Id.new i, initCoords )) acc

                else
                    mapHand ((::) (Id.new i)) acc
            )
            (LocationStore { table = [], hand = [] })


mapTable : (Table -> Table) -> LocationStore -> LocationStore
mapTable f l =
    mapContents (\locationStore -> { locationStore | table = f locationStore.table }) l


mapHand : (List Id -> List Id) -> LocationStore -> LocationStore
mapHand f l =
    mapContents (\locationStore -> { locationStore | hand = f locationStore.hand }) l


keys : LocationStore -> List Id
keys (LocationStore locationStore) =
    locationStore.table
        |> List.map Tuple.first
        |> (++) locationStore.hand
        |> List.sortBy show


toList : LocationStore -> List ( Id, Location )
toList locationStore =
    (handCards locationStore |> List.map (Tuple.mapSecond InHand))
        ++ (tableCards locationStore |> List.map (Tuple.mapSecond Table))


values : LocationStore -> List Location
values =
    toList >> List.map Tuple.second


insertAtIndex : Int -> b -> List b -> List b
insertAtIndex i item list =
    List.Extra.splitAt i list
        |> (\( front, back ) -> List.concat [ front, [ item ], back ])


setLocation : Id -> Location -> LocationStore -> LocationStore
setLocation id location =
    mapTable (List.filter (Tuple.first >> (/=) id))
        >> mapHand (List.filter ((/=) id))
        >> (case location of
                InHand i ->
                    mapHand (insertAtIndex i id)

                Table c ->
                    mapTable ((::) ( id, c ))
           )


type LocationStore
    = LocationStore LocationStoreRecord


type alias LocationStoreRecord =
    { hand : List Id
    , table : List ( Id, Coords )
    }


type Location
    = Table Coords
    | InHand HandPosition


type alias HandPosition =
    Int


type alias Table =
    List ( Id, Coords )


type alias Hand =
    List ( Id, HandPosition )


tableCards : LocationStore -> Table
tableCards (LocationStore locationStore) =
    locationStore.table


handCards : LocationStore -> Hand
handCards (LocationStore locationStore) =
    locationStore.hand
        |> List.indexedMap (\i id -> ( id, i ))


mapContents : (LocationStoreRecord -> LocationStoreRecord) -> LocationStore -> LocationStore
mapContents f (LocationStore locationStore) =
    LocationStore (f locationStore)


placeCard : Id -> Location -> LocationStore -> LocationStore
placeCard =
    setLocation
