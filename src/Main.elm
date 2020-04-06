module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseUp)
import Debug exposing (todo)
import Element exposing (centerY, el, explain, fill, height, inFront, moveDown, moveRight, none, padding, rgb, text, width)
import Element.Background exposing (color)
import Html exposing (Html)
import Json.Decode exposing (succeed)
import Mouse exposing (Coords, initCoords, onMouseCoords, onMouseDownCoords)



---- MODEL ----


type alias Model =
    { cardPosition : Coords
    , startDragCoords : Maybe Coords
    }


init : ( Model, Cmd Msg )
init =
    ( { cardPosition = initCoords
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
                , startDragCoords =
                    Maybe.map2
                        (\_ newMoving -> newMoving)
                        --don't move if the mouse has already been released (events arrive unordered)
                        model.startDragCoords
                        (Just moveRecord.current)
              }
            , Cmd.none
            )


cardPosition : Coords -> MouseRecord -> Coords
cardPosition previous { start, current } =
    { x = previous.x + (current.x - start.x)
    , y = previous.y + (current.y - start.y)
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , explain todo
        , inFront <| helloBanner
        , inFront <| draggableCard model
        ]
        none


helloBanner =
    Element.el
        [ width fill, centerY, color (rgb 0.8 0.4 0.4), padding 30 ]
        (Element.text "hello from elm-ui")


draggableCard model =
    el (cardStyles model.startDragCoords model.cardPosition) (text "drag me")


cardStyles startDragCoords { x, y } =
    [ padding 100, moveRight (toFloat x), moveDown (toFloat y) ]
        ++ (case startDragCoords of
                Just coords ->
                    [ color (rgb 0.8 0.8 0.4)
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
