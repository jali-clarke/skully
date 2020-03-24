{-# LANGUAGE GADTs #-}

module Skully (
    Skully(..),
    eval
) where

import CharSocket

data Skully a where
    S :: Skully ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: Skully (a -> b -> a)
    U :: Skully (Char -> a -> a)
    L :: Skully ((Char -> a) -> a)
    Y :: Skully ((a -> a) -> a)
    Q :: Skully (Char -> (Char -> Char -> a) -> a)
    Ap :: Skully (a -> b) -> Skully a -> Skully b
    Char :: Char -> Skully Char

show' :: Skully a -> (String -> String)
show' skully =
    case skully of
        S -> ('s' :)
        K -> ('k' :)
        U -> ('u' :)
        L -> ('l' :)
        Y -> ('y' :)
        Q -> ('q' :)
        Ap ex0 ex1 ->
            show' ex0 . case ex1 of
                Ap _ _ -> ('(' :) . show' ex1 . (')' :)
                _ -> show' ex1
        Char x -> (x :)

instance Show (Skully a) where
    show skully = show' skully ""

eval :: CharSocket m => Skully a -> m a
eval skully =
    case skully of
        S -> pure (\abc ab a -> abc a (ab a))
        K -> pure (\a _ -> a)
        _ -> undefined