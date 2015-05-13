module Common.Bools
    ( check
    ) where

check : (a -> Bool) -> b -> c
check f b c a = if f a then b else c