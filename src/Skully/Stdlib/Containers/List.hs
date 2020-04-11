module Skully.Stdlib.Containers.List (
    List
) where

type List a b = b -> (a -> b -> b) -> b 
