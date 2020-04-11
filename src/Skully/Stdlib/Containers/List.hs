module Skully.Stdlib.Containers.List (
    List,

    nil,
    cons,

    withList
) where

import Skully.Base

type List b a = b -> (a -> b -> b) -> b

nil :: Skully (List b a)
nil = undefined

cons :: Skully (a -> List b a -> List b a)
cons = undefined

withList :: Skully (List b a -> b -> (a -> b -> b) -> b)
withList = k .$ k
