module Main exposing (..)

import Browser
import Element exposing (centerY, el, fill, height, moveRight, padding, rgb, spacing, text, width)
import Element.Background exposing (color)
import Element.Events exposing (onMouseDown, onMouseMove, onMouseUp)
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { isMouseDown : Bool
    , cardPosition : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { isMouseDown = False
      , cardPosition = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | MouseUp
    | MouseDown
    | MouseMove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { model | isMouseDown = False }, Cmd.none )

        MouseMove ->
            ( { model | isMouseDown = False }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ width fill, height fill ]
            [ el (cardStyles model.isMouseDown model.cardPosition) (text "click me")
            , Element.el
                [ width fill, centerY, color (rgb 0.8 0.4 0.4), padding 30 ]
                (Element.text "hello from elm-ui")
            ]


cardStyles isMouseDown x =
    [ padding 100, moveRight x ]
        ++ (if isMouseDown then
                [ color (rgb 0.8 0.8 0.4)
                , onMouseUp MouseUp
                , onMouseMove MouseMove -- probably need a custom mouseMove to listen for x,y changes
                ]

            else
                [ color (rgb 0.8 0.4 0.8), onMouseDown MouseDown ]
           )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
