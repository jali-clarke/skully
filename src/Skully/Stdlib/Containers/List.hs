module Skully.Stdlib.Containers.List (
    List,

    nil,
    cons,
    reverse,

    withList
) where

import Prelude hiding (reverse)

import Skully.Base
import Skully.Stdlib.Combinators

type List b a = b -> (a -> b -> b) -> b
type DiffList b a = List b a -> List b a

nil :: Skully (List b a)
nil = k

cons :: Skully (a -> List b a -> List b a)
cons = c .$ c .$ (c .$ s .$ a)

reverseDiff :: Skully (List (DiffList b a) a -> DiffList b a)
reverseDiff = f .$ (a .$ i) .$ (c .$ (f .$ c) .$ cons)

reverse :: Skully (List (DiffList b a) a -> List b a)
reverse = f .$ reverseDiff .$ nil

withList :: Skully (List b a -> b -> (a -> b -> b) -> b)
withList = i
