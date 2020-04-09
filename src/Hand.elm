module Hand exposing (..)

import Card
import Element exposing (Attribute, Element, alignBottom, centerX, el, none, rotate, row)
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
    el [ alignBottom, centerX ] <|
        row [] <|
            List.indexedMap (\i c -> Card.view draggingCard c |> el [ cardAngle i (List.length hand) ]) hand


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
