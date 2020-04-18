module Card exposing (..)

import Element exposing (Element, centerX, centerY, el, height, inFront, moveDown, moveRight, none, px, rgb, text, width)
import Element.Background exposing (color)
import Element.Border as Border exposing (shadow)
import Id exposing (Id)
import Location exposing (Location(..))
import Model exposing (Card)
import Mouse exposing (Coords, initCoords, onMouseDownCoords)
import Msg exposing (Msg(..))


cardSize =
    200


mouseGrabPoint coords =
    { coords
        | x = coords.x - (cardSize // 2)
        , y = coords.y - (cardSize * 3 // 4)
    }


initCards : Int -> List Card
initCards count =
    let
        ids =
            List.range 1 count |> List.map Id.new

        locations =
            List.range 1 count
                |> List.map
                    (\i ->
                        if modBy 2 i == 0 then
                            Table initCoords

                        else
                            InHand
                    )
    in
    List.map2 Card
        ids
        locations


view : Maybe Card -> Card -> Element Msg
view startDragCard card =
    el (cardStyles startDragCard card) none


cardStyles : Maybe Card -> Card -> List (Element.Attribute Msg)
cardStyles startDragCard card =
    let
        isDragging =
            case startDragCard of
                Just draggingCard ->
                    draggingCard.id == card.id

                Nothing ->
                    False

        moveAttribute =
            case card.location of
                Table { x, y } ->
                    [ moveRight (toFloat x), moveDown (toFloat y) ]

                InHand ->
                    []

        showId =
            text (Id.show card.id)
                |> el [ centerX, centerY ]
                |> inFront
    in
    [ height (px cardSize), width (px cardSize), Border.rounded 15, showId ]
        ++ moveAttribute
        ++ (if isDragging then
                [ color (rgb 0.8 0.8 0.4)
                , shadow { offset = ( 0, 0 ), size = 0.0001, blur = 10, color = rgb 0.0 0.0 0.0 }
                ]

            else
                [ color (rgb 0.8 0.4 0.8)
                , onMouseDownCoords (MouseDown << Card card.id << Table)
                , shadow { offset = ( 0, 0 ), size = 0.000001, blur = 0.5, color = rgb 0.0 0.0 0.0 }
                ]
           )
