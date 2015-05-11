module Common.Tuple where

map : (a -> b) -> (a, a) -> (b, b)
map f (a1, a2) = (f a1, f a2)