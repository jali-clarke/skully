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

import Prelude hiding (getChar, putChar)

import Skully.CharSocket
import Skully.Type

eval :: CharSocket m => Skully a -> m (Skully a)
eval expr =
    case expr of
        Ap (Ap K a) _ -> eval a
        Ap (Ap U (Char c)) a -> putChar c *> eval a -- not complete
        Ap L g -> getChar >>= (\c -> eval (Ap g (Char c)))
        _ -> pure expr

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