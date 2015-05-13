module Response
    ( Result
    , resultDecoder
    ) where

import Json.Decode as Decode exposing (Decoder, (:=))

type alias Result a =
    { success : Bool
    , data    : a
    , errors  : List String
    }

resultDecoder : Decoder a -> Decoder (Result a)
resultDecoder dataDecoder =
    Decode.object3 Result
        ("success" := Decode.bool)
        ("data"    := dataDecoder)
        ("errors"  := Decode.list Decode.string)