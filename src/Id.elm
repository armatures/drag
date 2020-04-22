module Id exposing (Id, new, show, toInt_test)

import String exposing (fromInt)


type Id
    = Id Int


new : Int -> Id
new i =
    Id i


show (Id i) =
    fromInt i


toInt_test : Id -> Int
toInt_test (Id id) =
    id
