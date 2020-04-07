module Id exposing (Id, new)


type Id
    = Id Int


new : Int -> Id
new i =
    Id i
