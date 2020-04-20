module Hand exposing (..)

import Card
import Element exposing (Attribute, Element, rotate)
import Id exposing (Id)
import Model exposing (Card)
import Msg exposing (Msg(..))


view : Maybe Id -> Int -> Int -> Card -> Element Msg
view draggingCard handSize index card =
    let
        rotateCard i c =
            if isDragging draggingCard c then
                []

            else
                [ cardAngle i handSize ]
    in
    Card.view draggingCard card (rotateCard index card)


isDragging draggingCard card =
    case draggingCard of
        Just id ->
            card.id == id

        Nothing ->
            False


type alias Index =
    Int


type alias HandSize =
    Int


cardAngle : Index -> HandSize -> Attribute msg
cardAngle i size =
    if size > 1 then
        rotate << degrees <| ((toFloat i / toFloat (size - 1)) * 40) - 20

    else
        rotate 0
