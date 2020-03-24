{-# LANGUAGE GADTs #-}

module Skully (
    Skully(..)
) where

data Skully a where
    S :: Skully ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: Skully (a -> b -> a)
    U :: Skully (Char -> a -> a)
    L :: Skully ((Char -> a) -> a)
    Y :: Skully ((a -> a) -> a)
    Q :: Skully (Char -> (Char -> Char -> a) -> a)
    Ap :: Skully (a -> b) -> Skully a -> Skully b

instance Show (Skully a) where
    show skully =
        case skully of
            S -> "s"
            K -> "k"
            U -> "u"
            L -> "l"
            Y -> "y"
            Q -> "q"
            Ap ex0 ex1 ->
                show ex0 ++ " " ++ case ex1 of
                    Ap _ _ -> "(" ++ show ex1 ++ ")"
                    _ -> show ex1