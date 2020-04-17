module Skully.Stdlib.Containers.Pair (
    Pair,

    withPair,
    pair,

    fst,
    snd,
    swap
) where

import Prelude hiding (fst, snd)

import Skully.Base
import Skully.Stdlib.Combinators

type Pair r a b = (a -> b -> r) -> r

withPair :: Skully (Pair r a b -> (a -> b -> r) -> r)
withPair = i

pair :: Skully (a -> b -> Pair r a b)
pair = c .$ f .$ a

fst :: Skully (Pair a a b -> a)
fst = a .$ k

snd :: Skully (Pair b a b -> b)
snd = a .$ (f .$ k)

swap :: Skully (Pair (Pair r b a) a b -> Pair r b a)
swap = a .$ (f .$ pair)
