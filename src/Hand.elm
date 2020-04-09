module Hand exposing (..)

import Card
import Element exposing (Attribute, Element, alignBottom, centerX, el, rotate, row)
import Model exposing (Card)
import Msg exposing (Msg)


type Hand
    = Hand (List Card)


fromList : List Card -> Hand
fromList =
    Hand


draw : Card -> Hand -> Hand
draw card (Hand hand) =
    Hand (card :: hand)


view : Maybe Card -> Hand -> Element Msg
view draggingCard (Hand hand) =
    let
        rotateCard i c =
            if isDragging draggingCard c then
                identity

            else
                el [ cardAngle i (List.length hand) ]
    in
    el [ alignBottom, centerX ] <|
        row [] <|
            List.indexedMap (\i c -> rotateCard i c <| Card.view draggingCard c) hand


isDragging draggingCard card =
    case draggingCard of
        Just c ->
            card.id == c.id

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
