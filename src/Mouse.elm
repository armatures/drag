module Mouse exposing (..)

import Element exposing (Attribute, htmlAttribute)
import Html.Events exposing (on)
import Json.Decode as Json


type alias Coords =
    { x : Int
    , y : Int
    }


initCoords =
    { x = 0, y = 0 }


{-| -}
localCoords : Json.Decoder Coords
localCoords =
    Json.map2 Coords
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)


pageCoords : Json.Decoder Coords
pageCoords =
    Json.map2 Coords
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)


screenCoords : Json.Decoder Coords
screenCoords =
    Json.map2 Coords
        (Json.field "screenX" Json.int)
        (Json.field "screenY" Json.int)



--onClickCoords msg =
--    on "click" (Json.map msg localCoords)


onMouseDownCoords : (Coords -> msg) -> Attribute msg
onMouseDownCoords msg =
    htmlAttribute <| on "mousedown" (Json.map msg pageCoords)


{-| -}
onMouseCoords : (Coords -> msg) -> Attribute msg
onMouseCoords msg =
    htmlAttribute <| on "mousemove" (Json.map msg pageCoords)
