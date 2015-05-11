module Response where

type alias Result a =
    { success : Bool
    , data    : a
    , errors  : List String
    }