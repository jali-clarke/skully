module Skully.Stdlib.Containers.List (
    List,

    nil,
    cons
) where

import Skully.Base
import Skully.Stdlib.Combinators

type List b a = b -> (a -> b -> b) -> b 

nil :: Skully (List b a)
nil = k

cons :: Skully (a -> List b a -> List b a)
cons = c .$ c .$ (c .$ s .$ a)
