{-# LANGUAGE GADTs #-}

module Skully.Internal.TypeRep (
    TypeRep(..),
    unify
) where

data TypeRep a where
    Char :: TypeRep Char

instance Eq (TypeRep a) where
    Char == Char = True

instance Show (TypeRep a) where
    show Char = "Char"

unify :: TypeRep a -> TypeRep b -> Either String (TypeRep a)
unify Char Char = Right Char
