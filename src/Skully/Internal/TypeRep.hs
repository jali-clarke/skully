{-# LANGUAGE GADTs #-}

module Skully.Internal.TypeRep (
    TypeRep(..),
    unify
) where

data TypeRep a where
    Char :: TypeRep Char
    (:->:) :: TypeRep a -> TypeRep b -> TypeRep (a -> b)

instance Eq (TypeRep a) where
    Char == Char = True
    (Char :->: Char) == (Char :->: Char) = True
    _ == _ = False

instance Show (TypeRep a) where
    show Char = "Char"
    show _ = "Char :->: Char"

unify :: TypeRep a -> TypeRep b -> Either String (TypeRep a)
unify Char Char = Right Char
unify (Char :->: Char) (Char :->: Char) = Right (Char :->: Char)
unify a b =
    let aString =
            case a of
                (_ :->: _) -> "(" ++ show a ++ ")"
                _ -> show a
        bString =
            case b of
                _ :->: _ -> "(" ++ show b ++ ")"
                _ -> show b
    in Left ("cannot unify " ++ aString ++ " against " ++ bString)
