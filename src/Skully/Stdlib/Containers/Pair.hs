module Skully.Stdlib.Containers.Pair (
    Pair,

    withPair,
    pair,

    fst
) where

import Prelude hiding (fst)

import Skully.Base
import Skully.Stdlib.Combinators

type Pair r a b = (a -> b -> r) -> r

withPair :: Skully (Pair r a b -> (a -> b -> r) -> r)
withPair = i

pair :: Skully (a -> b -> Pair r a b)
pair = c .$ f .$ a

fst :: Skully (Pair a a b -> a)
fst = a .$ k
