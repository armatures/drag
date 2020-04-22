module Tests exposing (..)

import Expect
import Id
import Location exposing (Location(..), LocationStore(..), cardLocation, initLocationStore, keys, placeCard, values)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    let
        testLocationStore =
            initLocationStore 4

        firstId =
            Id.new 1

        tableLocation =
            Table { x = 3, y = 3 }
    in
    describe "Location"
        [ describe "initLocationStore"
            [ test "can have a single card" <|
                \_ ->
                    initLocationStore 1
                        |> keys
                        |> List.length
                        |> Expect.equal 1
            , test "gives the first card 'firstId'" <|
                \_ ->
                    initLocationStore 1
                        |> keys
                        |> Expect.equal [ firstId ]
            ]
        , describe "placeCard"
            [ test "sets a card's location" <|
                \_ ->
                    placeCard firstId tableLocation testLocationStore
                        |> cardLocation firstId
                        |> Expect.equal (Just tableLocation)
            , todo "does not duplicate hand positions"
            , test "can duplicate table coords" <|
                \_ ->
                    initLocationStore 2
                        |> placeCard (Id.new 1) tableLocation
                        |> placeCard (Id.new 2) tableLocation
                        |> values
                        |> Expect.equal [ tableLocation, tableLocation ]
            ]
        ]
