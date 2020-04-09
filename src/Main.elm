module Main exposing (..)

import AssocList as Dict exposing (Dict)
import Browser
import Browser.Events exposing (onMouseUp)
import Card exposing (cardSize, initCards)
import Element exposing (Element, centerY, fill, height, inFront, none, padding, rgb, width)
import Element.Background exposing (color)
import Hand
import Html exposing (Html)
import Id exposing (Id)
import Json.Decode exposing (succeed)
import Model exposing (Card, Location(..), mapLocation)
import Mouse exposing (Coords, subMouseMoveCoords)
import Msg exposing (DragRecord, Msg(..))



---- MODEL ----


type alias Model =
    { cards : Dict Id Card
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
                | cards = Dict.map (cardPosition moveRecord) model.cards
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


{-| this won't work... Right now all the cards rely on starting at the origin, and I'm not sure how to
get the appropriate offset from the origin when cards are coming from their hand.
we need to act differently when transitioning hand cards onto the table.
mouse coordinates into pageCoordinates is just the mouse position listener we have
-}
cardPosition : DragRecord -> Id -> Card -> Card
cardPosition { startId, startCoords, current } id previous =
    if id == startId then
        mapLocation
            (\location ->
                case location of
                    --could probably dry these up
                    Table previousLocation ->
                        Table
                            { x = previousLocation.x + (current.x - startCoords.x)
                            , y = previousLocation.y + (current.y - startCoords.y)
                            }

                    InHand ->
                        Table
                            { x = current.x - startCoords.x
                            , y = current.y - startCoords.y
                            }
            )
            previous

    else
        previous



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cardList =
            model.cards
                |> Dict.toList
                |> List.map Tuple.second

        cardsAsAttributes =
            List.map (Card.view model.draggingCard) (List.filter (.location >> (==) InHand >> not) cardList)
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
                        (Hand.fromList (List.filter (.location >> (==) InHand) cardList))
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
                , startCoords =
                    case c.location of
                        Table tableCoords ->
                            tableCoords

                        InHand ->
                            { x = 0, y = 0 }
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
