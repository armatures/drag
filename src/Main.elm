module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseUp)
import Element exposing (centerY, el, fill, height, inFront, moveDown, moveRight, none, padding, px, rgb, text, width)
import Element.Background exposing (color)
import Html exposing (Html)
import Json.Decode exposing (succeed)
import Mouse exposing (Coords, onMouseCoords, onMouseDownCoords)



---- MODEL ----


type alias Model =
    { cardPosition : Float
    , startDragCoords : Maybe Coords
    }


init : ( Model, Cmd Msg )
init =
    ( { cardPosition = 0
      , startDragCoords = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | MouseUp
    | MouseDown Coords
    | MouseMove MouseRecord


type alias MouseRecord =
    { start : Coords, current : Coords }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown coords ->
            ( { model | startDragCoords = Just coords }, Cmd.none )

        MouseUp ->
            ( { model | startDragCoords = Nothing }, Cmd.none )

        MouseMove moveRecord ->
            ( { model
                | cardPosition = cardPosition model.cardPosition moveRecord
                , startDragCoords = Just moveRecord.current
              }
            , Cmd.none
            )


cardPosition : Float -> MouseRecord -> Float
cardPosition previous { start, current } =
    previous
        + toFloat (current.x - start.x)



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ width fill, height fill ]
            [ el (cardStyles model.startDragCoords model.cardPosition) (text "click me")
            , Element.el
                [ width fill, centerY, color (rgb 0.8 0.4 0.4), padding 30 ]
                (Element.text "hello from elm-ui")
            ]


cardStyles startDragCoords x =
    [ padding 100, moveRight x ]
        ++ (case startDragCoords of
                Just coords ->
                    [ color (rgb 0.8 0.8 0.4)
                    , onMouseCoords (\c -> MouseMove { start = coords, current = c })
                    ]

                Nothing ->
                    [ color (rgb 0.8 0.4 0.8), onMouseDownCoords MouseDown ]
           )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always (onMouseUp (succeed MouseUp))
        }
