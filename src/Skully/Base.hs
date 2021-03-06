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

l :: Skully (a -> (Char -> a) -> a)
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
    let evalInner :: CharSocket m => Skully a -> m (Skully a)
        evalInner expr' = 
            case expr' of
                Ap (Ap (Ap S abc) ab) a -> evalInner (Ap (Ap abc a) (Ap ab a))
                Ap (Ap K a) _ -> evalInner a
                Ap (Ap U c) a -> do
                    c' <- evalInner c
                    case c' of
                        Char x -> putChar x *> evalInner a
                        _ -> undefined -- will never be reached
                Ap (Ap L d) g -> do
                    x <- getChar
                    case x of
                        Nothing -> evalInner d
                        Just x' -> evalInner (Ap g (Char x'))
                Ap Y g -> evalInner (Ap g (Ap Y g))
                Ap (Ap Q c) g -> do
                    c' <- evalInner c
                    case c' of
                        Char x -> evalInner (Ap (Ap g (Char (predChar x))) (Char (succChar x)))
                        _ -> evalInner (Ap (Ap Q c') g)
                Ap (Ap (Ap (Ap (Ap E c0) c1) a) b) c -> do
                    (c0', c1') <- (,) <$> evalInner c0 <*> evalInner c1
                    case (c0', c1') of
                        (Char x0, Char x1) -> evalInner $ select (x0 `compare` x1) a b c
                        _ -> undefined -- will never be reached
                Ap S _ -> pure expr'
                Ap (Ap S _) _ -> pure expr'
                Ap K _ -> pure expr'
                Ap U _ -> pure expr'
                Ap L _ -> pure expr'
                Ap Q _ -> pure expr'
                Ap E _ -> pure expr'
                Ap (Ap E _) _ -> pure expr'
                Ap (Ap (Ap E _) _) _ -> pure expr'
                Ap (Ap (Ap (Ap E _) _) _) _ -> pure expr'
                Ap a b -> do
                    a' <- evalInner a
                    evalInner (Ap a' b)
                _ -> pure expr'

    in initSocket *> evalInner expr

optimizeStep :: Skully a -> Skully a
optimizeStep expr =
    case expr of
        Ap (Ap (Ap S abc) ab) a -> Ap (Ap abc a) (Ap ab a)
        Ap (Ap K a) _ -> a
        Ap (Ap Q c) g ->
            let c' = optimize c
            in case c' of
                Char x -> Ap (Ap g (Char (predChar x))) (Char (succChar x))
                _ -> Ap (Ap Q c') (optimize g)
        Ap (Ap (Ap (Ap (Ap E c0) c1) a) b) c ->
            let cs@(c0', c1') = (optimize c0, optimize c1)
            in case cs of
                (Char x0, Char x1) -> select (x0 `compare` x1) a b c
                _ -> Ap (Ap (Ap (Ap (Ap E c0') c1') (optimize a)) (optimize b)) (optimize c)
        Ap Y g ->
            let g' = optimize g
            in case g' of
                Ap K a -> a
                _ -> Ap Y g'
        Ap a b -> Ap (optimize a) (optimize b)
        _ -> expr

optimize :: Skully a -> Skully a
optimize expr =
    let expr' = optimizeStep expr
    in if expr == expr' then expr else optimize expr'

select :: Ordering -> a -> a -> a -> a
select ord a b c =
    case ord of
        LT -> a
        EQ -> b
        GT -> c

predChar :: Char -> Char
predChar x = if x == '\x00' then '\xff' else pred x

succChar :: Char -> Char
succChar x = if x == '\xff' then '\x00' else succ x
