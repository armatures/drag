module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseUp)
import Card exposing (cardSize)
import Element exposing (Element, alignBottom, centerX, centerY, el, fill, height, inFront, moveDown, moveRight, none, padding, rgb, row, width)
import Element.Background exposing (color)
import Element.Events
import Hand
import Html exposing (Html)
import Id exposing (Id)
import Json.Decode exposing (succeed)
import Location exposing (Location(..), LocationStore, initLocationStore, keys, mapLocation)
import Model exposing (Card, Model, mapLocationStore)
import Mouse exposing (Coords, subMouseMoveCoords)
import Msg exposing (DragRecord, Msg(..))



---- MODEL ----


init : ( Model, Cmd Msg )
init =
    let
        locationStore =
            initLocationStore 4
    in
    ( { cards = keys locationStore |> List.map Card
      , draggingCard = Nothing
      , locationStore = locationStore
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown card _ ->
            ( { model
                | draggingCard =
                    Just
                        { id = card.id
                        , location =
                            Table
                                { x = cardSize // 2
                                , y = cardSize // 2
                                }
                        }
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | draggingCard = Nothing }, Cmd.none )

        MouseMove moveRecord ->
            let
                newDraggingCard =
                    case model.draggingCard of
                        Nothing ->
                            Nothing

                        Just _ ->
                            Just
                                { id = moveRecord.startId
                                , location = Table <| Card.mouseGrabPoint moveRecord.current
                                }
            in
            ( { model
                | draggingCard = newDraggingCard
              }
                |> mapLocationStore
                    (mapLocation moveRecord.startId
                        (always (newDraggingCard |> Maybe.map .location))
                    )
            , Cmd.none
            )

        MouseUpOnHand ->
            case model.draggingCard of
                Nothing ->
                    ( model, Cmd.none )

                Just dragging ->
                    ( mapLocationStore (mapLocation dragging.id (always (Just InHand))) model
                    , Cmd.none
                    )


mapCardWithId : Id -> (Card -> Card) -> List Card -> List Card
mapCardWithId id f =
    List.map
        (\card ->
            if card.id == id then
                f card

            else
                card
        )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cardList =
            model.cards

        viewTableCards : List (Element Msg)
        viewTableCards =
            Location.tableCards cardList model.locationStore
                |> List.map showTableCard

        draggingCardId =
            Maybe.map .id model.draggingCard

        showTableCard : ( Card, Coords ) -> Element Msg
        showTableCard ( card, { x, y } ) =
            Card.view draggingCardId card [ moveRight (toFloat x), moveDown (toFloat y) ]

        viewHandCards : List Card -> Element Msg
        viewHandCards ids =
            el [ alignBottom, centerX, Element.Events.onMouseUp MouseUpOnHand ] <|
                row [] <|
                    List.indexedMap (Hand.view draggingCardId (List.length ids)) ids

        cardsAsAttributes =
            List.map inFront
                (viewTableCards ++ [ viewHandCards (Location.handCards cardList model.locationStore) ])
    in
    Element.layout
        ([ width fill
         , height fill
         , inFront helloBanner
         ]
            ++ cardsAsAttributes
        )
        none


helloBanner =
    Element.el
        [ width fill, centerY, color (rgb 0.8 0.4 0.4), padding 30 ]
        (Element.text "hello from elm-ui")



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        f : { a | id : Id } -> Coords -> Msg
        f c coords =
            MouseMove
                { startId = c.id
                , current = coords
                }
    in
    onMouseUp (succeed MouseUp)
        :: (case model.draggingCard of
                Just card ->
                    [ subMouseMoveCoords (f card) ]

                Nothing ->
                    []
           )
        |> Sub.batch
