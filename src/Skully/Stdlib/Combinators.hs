module Skully.Stdlib.Combinators (
    module Skully,

    a,
    c,
    d,
    f,
    i
) where

import Skully

a :: Skully (a -> (a -> b) -> b)
a = f .$ i

c :: Skully ((b -> c) -> (a -> b) -> a -> c)
c = s .$ (k .$ s) .$ k

d :: Skully ((a -> a -> b) -> a -> b)
d = f .$ s .$ i

f :: Skully ((a -> b -> c) -> b -> a -> c)
f = s .$ (c .$ c .$ s) .$ (k .$ k)

i :: Skully (a -> a)
i = s .$ k .$ k
