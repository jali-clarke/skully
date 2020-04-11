{-# LANGUAGE GADTs #-}

module Skully.Internal.Type (
    Skully(..),
    eval
) where

import Prelude hiding (getChar, putChar)
import Data.Char (chr, ord)

import Skully.Internal.CharSocket

data Skully a where
    S :: Skully ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: Skully (a -> b -> a)
    U :: Skully (Char -> a -> a)
    L :: Skully ((Char -> a) -> a)
    Y :: Skully ((a -> a) -> a)
    Q :: Skully (Char -> (Char -> Char -> a) -> a)
    E :: Skully (Char -> Char -> a -> a -> a -> a)
    Ap :: Skully (a -> b) -> Skully a -> Skully b
    Char :: Char -> Skully Char

instance Show (Skully a) where
    show skully = show' skully ""

show' :: Skully a -> (String -> String)
show' skully =
    case skully of
        S -> ('s' :)
        K -> ('k' :)
        U -> ('u' :)
        L -> ('l' :)
        Y -> ('y' :)
        Q -> ('q' :)
        E -> ('e' :)
        Ap ex0 ex1 ->
            show' ex0 . case ex1 of
                Ap _ _ -> ('(' :) . show' ex1 . (')' :)
                _ -> show' ex1
        Char c ->
            let showCharAsHexIfNecessary =
                    if c < '\x20' || c > '\x7e'
                        then ('\\' :) . ('x' :) . showHex (ord c)
                        else (c :)
            in ('\'' :) . showCharAsHexIfNecessary . ('\'' :)

showHex :: Int -> (String -> String)
showHex n =
    let (d, m) = n `divMod` 16
        toHexChar x = if x < 10 then (chr (x + 48) :) else (chr (x + 87) :)
    in toHexChar d . toHexChar m

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