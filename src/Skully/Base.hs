{-# LANGUAGE GADTs #-}

module Skully.Base (
    Skully,

    eval,
    optimize,

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

import Skully.Internal.CharSocket
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
                Char x -> eval (Ap (Ap g (Char (predChar x))) (Char (succChar x)))
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

optimize :: Skully a -> Skully a
optimize expr =
    case expr of
        Ap (Ap (Ap S abc) ab) a -> optimize (Ap (Ap abc a) (Ap ab a))
        Ap (Ap K a) _ -> optimize a
        Ap (Ap Q c) g ->
            let c' = optimize c
            in case c' of
                Char x -> optimize (Ap (Ap g (Char (predChar x))) (Char (succChar x)))
                _ -> Ap (Ap Q c') (optimize g)
        Ap S abc -> Ap S (optimize abc)
        Ap (Ap S abc) ab -> Ap (Ap S (optimize abc)) (optimize ab)
        Ap K a -> Ap K (optimize a)
        Ap U c -> Ap U (optimize c)
        Ap (Ap U c) a -> Ap (Ap U (optimize c)) (optimize a)
        Ap L g -> Ap L (optimize g)
        Ap Q c -> Ap Q (optimize c)
        _ -> expr

predChar :: Char -> Char
predChar x = if x == '\x00' then '\xff' else pred x

succChar :: Char -> Char
succChar x = if x == '\xff' then '\x00' else succ x
