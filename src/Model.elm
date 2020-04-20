module Model exposing (..)

import Id exposing (Id)
import Location exposing (Location(..), LocationStore)


type alias Card =
    { id : Id }


type alias Model =
    { cards : List Card
    , draggingCard : Maybe { id : Id, location : Location }
    , locationStore : LocationStore
    }


mapCoords f card =
    { card | coords = f card.coords }


mapCards f model =
    { model | cards = f model.cards }


mapLocationStore : (LocationStore -> LocationStore) -> Model -> Model
mapLocationStore f model =
    { model | locationStore = f model.locationStore }
