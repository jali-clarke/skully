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
    _ == _ = False

instance Show (TypeRep a) where
    show Char = "Char"
    show _ = "Char :->: Char"

unify :: TypeRep a -> TypeRep b -> Either String (TypeRep a)
unify Char Char = Right Char
unify _ _ = Left "cannot unify (Char :->: Char) against Char"
