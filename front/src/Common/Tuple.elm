module Common.Tuple
    ( map
    , all
    ) where

map : (a -> b) -> (a, a) -> (b, b)
map f (a1, a2) = (f a1, f a2)

all : (a -> Bool) -> (a, a) -> Bool
all f (a1, a2) = f a1 && f a2