module Main exposing (..)

import Browser
import Element exposing (centerY, fill, padding, rgb, spacing, width)
import Element.Background exposing (color)
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.el
            [width fill, centerY, color (rgb 0.8 0.4 0.4), padding 30]
         (Element.text "hello from elm-ui")



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
