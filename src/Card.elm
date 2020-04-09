module Card exposing (..)

import AssocList as Dict exposing (Dict)
import Element exposing (Element, el, moveDown, moveRight, padding, rgb, text)
import Element.Background exposing (color)
import Element.Border as Border exposing (shadow)
import Id exposing (Id)
import Model exposing (Card)
import Mouse exposing (Coords, initCoords, onMouseDownCoords)
import Msg exposing (Msg(..))


initCards : Int -> Dict Id Card
initCards count =
    let
        ids =
            List.range 1 count |> List.map Id.new
    in
    List.map2 Card
        ids
        (List.repeat count initCoords)
        |> List.map2 Tuple.pair ids
        |> Dict.fromList


draggableCard : Maybe Card -> Card -> Element Msg
draggableCard startDragCard card =
    el (cardStyles startDragCard card) (text "drag me")


cardStyles startDragCard card =
    let
        isDragging =
            case startDragCard of
                Just draggingCard ->
                    draggingCard.id == card.id

                Nothing ->
                    False
    in
    [ padding 100, moveRight (toFloat card.coords.x), moveDown (toFloat card.coords.y), Border.rounded 15 ]
        ++ (if isDragging then
                [ color (rgb 0.8 0.8 0.4)
                , shadow { offset = ( 0, 0 ), size = 0.0001, blur = 10, color = rgb 0.0 0.0 0.0 }
                ]

            else
                [ color (rgb 0.8 0.4 0.8)
                , onMouseDownCoords (MouseDown << Card card.id)
                , shadow { offset = ( 0, 0 ), size = 0.000001, blur = 0.5, color = rgb 0.0 0.0 0.0 }
                ]
           )
