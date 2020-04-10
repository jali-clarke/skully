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
        Ap (Ap (Ap S abc) ab) a -> eval (Ap (Ap abc a) (Ap ab a))
        Ap (Ap K a) _ -> eval a
        Ap (Ap U c) a ->
            case c of
                Char x -> putChar x *> eval a
                _ -> eval c >>= (\c' -> eval (Ap (Ap U c') a))
        Ap L g -> getChar >>= (\x -> eval (Ap g (Char x)))
        Ap Y g -> eval (Ap g (Ap Y g))
        Ap (Ap Q (Char x)) g -> pure (Ap (Ap g (Char (pred x))) (Char (succ x)))
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