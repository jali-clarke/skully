{-# LANGUAGE GADTs #-}

module Skully.Internal.Type (
    Skully(..)
) where

import Data.Char (chr, ord)

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
