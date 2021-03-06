module Main exposing (..)

import AssocList as Dict exposing (Dict)
import Browser
import Browser.Events exposing (onMouseUp)
import Card exposing (initCards)
import Element exposing (Element, centerY, fill, height, inFront, none, padding, rgb, width)
import Element.Background exposing (color)
import Html exposing (Html)
import Id exposing (Id)
import Json.Decode exposing (succeed)
import Model exposing (Card)
import Mouse exposing (Coords, subMouseMoveCoords)
import Msg exposing (DragRecord, Msg(..))



---- MODEL ----


type alias Model =
    { cards : Dict Id Card
    , startDragCoords : Maybe Card
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = initCards 3
      , startDragCoords = Nothing
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
            ( { model | startDragCoords = Just card }, Cmd.none )

        MouseUp ->
            ( { model | startDragCoords = Nothing }, Cmd.none )

        MouseMove moveRecord ->
            ( { model
                | cards = Dict.map (cardPosition moveRecord) model.cards
                , startDragCoords =
                    Maybe.map2
                        (\_ newMoving -> newMoving)
                        --don't move if the mouse has already been released (events arrive unordered)
                        model.startDragCoords
                        (Just moveRecord.current)
                        |> Maybe.map (Card moveRecord.start.id)

                -- the "current" field doesn't have the id, but these must refer to the same record.
              }
            , Cmd.none
            )


cardPosition : DragRecord -> Id -> Card -> Card
cardPosition { start, current } id previous =
    if id == start.id then
        { id = id
        , coords =
            { x = previous.coords.x + (current.x - start.coords.x)
            , y = previous.coords.y + (current.y - start.coords.y)
            }
        }

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
            List.map (Card.view model.startDragCoords) cardList
                |> List.map inFront
    in
    Element.layout
        ([ width fill
         , height fill
         , inFront <| helloBanner
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
        f : Card -> Coords -> Msg
        f coords c =
            MouseMove { start = coords, current = c }
    in
    onMouseUp (succeed MouseUp)
        :: (case model.startDragCoords of
                Just card ->
                    [ subMouseMoveCoords (f card) ]

                Nothing ->
                    []
           )
        |> Sub.batch
