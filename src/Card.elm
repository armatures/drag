module Card exposing (..)

import Colors exposing (colors)
import Element exposing (Element, centerX, centerY, el, height, htmlAttribute, inFront, none, px, text, width)
import Element.Background exposing (color)
import Element.Border as Border exposing (shadow)
import Html.Attributes as A
import Id exposing (Id)
import Model exposing (Card)
import Mouse exposing (Coords, onMouseDownCoords)
import Msg exposing (Msg(..))


cardSize =
    200


mouseGrabPoint coords =
    { coords
        | x = coords.x - (cardSize // 2)
        , y = coords.y - (cardSize * 3 // 4)
    }


view : Maybe Id -> Card -> List (Element.Attribute Msg) -> Element Msg
view startDragCard card additionalStyles =
    el (cardStyles startDragCard card ++ additionalStyles) none


cardStyles : Maybe Id -> Card -> List (Element.Attribute Msg)
cardStyles startDragCard card =
    let
        isDragging =
            case startDragCard of
                Just draggingCard ->
                    draggingCard == card.id

                Nothing ->
                    False

        showId =
            text (Id.show card.id)
                |> el [ centerX, centerY ]
                |> inFront
    in
    [ height (px cardSize), width (px cardSize), Border.rounded 15, showId, htmlAttribute (A.class "no-select") ]
        ++ (if isDragging then
                [ color colors.cardBack
                , shadow { offset = ( 0, 0 ), size = 0.0001, blur = 10, color = colors.shadow }
                ]

            else
                [ color colors.cardBack
                , onMouseDownCoords (MouseDown (Card card.id))
                , shadow { offset = ( 0, 0 ), size = 0.000001, blur = 0.5, color = colors.shadow }
                ]
           )
