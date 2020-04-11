{-# LANGUAGE GADTs #-}

module Skully (
    Skully,

    eval,

    s,
    k,
    u,
    l,
    y,
    q,
    e,
    (.$),
    char
) where

import Skully.Internal.Type

s :: Skully ((a -> b -> c) -> (a -> b) -> a -> c)
s = S

k :: Skully (a -> b -> a)
k = K

u :: Skully (Char -> a -> a)
u = U

l :: Skully ((Char -> a) -> a)
l = L

y :: Skully ((a -> a) -> a)
y = Y

q :: Skully (Char -> (Char -> Char -> a) -> a)
q = Q

e :: Skully (Char -> Char -> a -> a -> a -> a)
e = E

infixl 9 .$
(.$) :: Skully (a -> b) -> Skully a -> Skully b
(.$) = Ap

char :: Char -> Skully Char
char = Char