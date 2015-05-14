module Validators.Text 
    ( notEmpty
    , toErrorList
    ) where

import Result exposing (Result)

notEmpty : String -> String -> Result String String
notEmpty message value =
    if (value == "") then Result.Err message else Result.Ok value

toErrorList : Result String String -> List String
toErrorList result =
    case result of
        Ok _      -> []
        Err error -> [error]