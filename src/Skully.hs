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
                _ -> do
                    c' <- eval c
                    eval (Ap (Ap U c') a)
        Ap L g -> do
            x <- getChar
            eval (Ap g (Char x))
        Ap Y g -> eval (Ap g (Ap Y g))
        Ap (Ap Q c) g ->
            case c of
                Char x ->
                    let predChar = if x == '\x00' then x else pred x
                        succChar = if x == '\xff' then x else succ x
                    in eval (Ap (Ap g (Char predChar)) (Char succChar))
                _ -> eval c >>= (\c' -> eval (Ap (Ap Q c') g))
        Ap (Ap (Ap (Ap (Ap E c0) c1) a) b) c ->
            case (c0, c1) of
                (Char x0, Char x1) ->
                    eval $ case x0 `compare` x1 of
                        LT -> a
                        EQ -> b
                        GT -> c
                _ -> do
                    c0' <- eval c0
                    c1' <- eval c1
                    eval (Ap (Ap (Ap (Ap (Ap E c0') c1') a) b) c)
        Ap S _ -> pure expr
        Ap (Ap S _) _ -> pure expr
        Ap K _ -> pure expr
        Ap U _ -> pure expr
        Ap Q _ -> pure expr
        Ap E _ -> pure expr
        Ap (Ap E _) _ -> pure expr
        Ap (Ap (Ap E _) _) _ -> pure expr
        Ap (Ap (Ap (Ap E _) _) _) _ -> pure expr
        Ap a b -> do
            a' <- eval a
            eval (Ap a' b)
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