module Validators.Text 
    ( notEmpty
    ) where

import Result exposing (Result)

notEmpty : String -> String -> Result String String
notEmpty message value =
    if (value == "") then Result.Err message else Result.Ok value