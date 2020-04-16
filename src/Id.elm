module Id exposing (Id, new, show)

import String exposing (fromInt)


type Id
    = Id Int


new : Int -> Id
new i =
    Id i


show (Id i) =
    fromInt i
