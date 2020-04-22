module Tests exposing (..)

import Expect
import Id
import Location exposing (Location(..), LocationStore(..), cardLocation, initLocationStore, keys, placeCard, toList_test, values)
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
            , test "does not duplicate hand positions, to make ordering easily maintainable" <|
                \_ ->
                    initLocationStore 4
                        |> placeCard (Id.new 1) (InHand 0)
                        |> placeCard (Id.new 2) (InHand 0)
                        |> placeCard (Id.new 3) (InHand 0)
                        |> placeCard (Id.new 4) (InHand 0)
                        |> toList_test
                        |> Expect.equal
                            [ ( 1, InHand 3 ), ( 2, InHand 2 ), ( 3, InHand 1 ), ( 4, InHand 0 ) ]
            , test "allows inserting into the middle of a hand, incrementing the later cards' indices" <|
                \_ ->
                    initLocationStore 4
                        |> placeCard (Id.new 1) (InHand 0)
                        |> placeCard (Id.new 2) (InHand 1)
                        |> placeCard (Id.new 3) (InHand 2)
                        |> placeCard (Id.new 4) (InHand 1)
                        |> toList_test
                        |> Expect.equal
                            [ ( 1, InHand 0 ), ( 2, InHand 2 ), ( 3, InHand 3 ), ( 4, InHand 1 ) ]
            , test "can duplicate table coords" <|
                \_ ->
                    initLocationStore 2
                        |> placeCard (Id.new 1) tableLocation
                        |> placeCard (Id.new 2) tableLocation
                        |> values
                        |> Expect.equal [ tableLocation, tableLocation ]
            ]
        ]
