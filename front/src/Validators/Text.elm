module Validators.Text where

notEmpty : String -> Result ()
notEmpty text = text