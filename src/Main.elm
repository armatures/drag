module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseUp)
import Card exposing (cardSize, initCards)
import Element exposing (Element, centerY, fill, height, inFront, none, padding, rgb, width)
import Element.Background exposing (color)
import Hand
import Html exposing (Html)
import Id exposing (Id)
import Json.Decode exposing (succeed)
import Location exposing (Location(..))
import Model exposing (Card, cardsInHand, mapCards, mapLocation, tableCards)
import Mouse exposing (Coords, subMouseMoveCoords)
import Msg exposing (DragRecord, Msg(..))



---- MODEL ----


type alias Model =
    { cards : List Card
    , draggingCard : Maybe Card
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = initCards 4
      , draggingCard = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown card ->
            ( { model
                | draggingCard =
                    Just
                        (mapLocation
                            (always <|
                                Table
                                    { x = cardSize // 2
                                    , y = cardSize // 2
                                    }
                            )
                            card
                        )
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | draggingCard = Nothing }, Cmd.none )

        MouseMove moveRecord ->
            ( { model
                | cards = List.map (cardPosition moveRecord) model.cards
                , draggingCard =
                    Maybe.map2
                        (\_ newMoving -> newMoving)
                        --don't move if the mouse has already been released (events arrive unordered)
                        model.draggingCard
                        (Just moveRecord.current)
                        |> Maybe.map (Card moveRecord.startId << Table)

                -- the "current" field doesn't have the id, but these must refer to the same record.
              }
            , Cmd.none
            )

        MouseUpOnHand ->
            case model.draggingCard of
                Nothing ->
                    ( model, Cmd.none )

                Just dragging ->
                    ( mapCards (mapCardWithId dragging.id (mapLocation (always InHand))) model
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


cardPosition : DragRecord -> Card -> Card
cardPosition { startId, current } previous =
    let
        newLocation =
            if previous.id == startId then
                case previous.location of
                    Table _ ->
                        Table (Card.mouseGrabPoint current)

                    InHand ->
                        Table (Card.mouseGrabPoint current)

            else
                previous.location
    in
    { previous | location = newLocation }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cardList =
            model.cards

        cardsAsAttributes =
            List.map (Card.view model.draggingCard) (tableCards cardList)
                |> List.map inFront
    in
    Element.layout
        ([ width fill
         , height fill
         , inFront helloBanner
         ]
            ++ cardsAsAttributes
            ++ [ inFront
                    (Hand.view model.draggingCard
                        (Hand.fromList (cardsInHand cardList))
                    )
               ]
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
        f : Card -> Coords -> Msg
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
