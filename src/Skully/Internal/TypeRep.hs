{-# LANGUAGE
    GADTs,
    RankNTypes
#-}

module Skully.Internal.TypeRep (
    TypeRep(..),
    SomeTypeRep(..),
    TypeMap,

    unify
) where

import qualified Data.IntMap.Strict as Map

data TypeRep a where
    Char :: TypeRep Char
    (:->:) :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
    Var :: Int -> TypeRep a

data SomeTypeRep = forall a. SomeTypeRep (TypeRep a)
type TypeMap = Map.IntMap SomeTypeRep

instance Eq (TypeRep a) where
    Char == Char = True
    (Char :->: Char) == (Char :->: Char) = True
    Var n0 == Var n1 = n0 == n1
    _ == _ = False

instance Show (TypeRep a) where
    show a = show' a ""

instance Eq SomeTypeRep where
    (SomeTypeRep a) == (SomeTypeRep b) =
        case unify a b Map.empty of
            Left _ -> False
            Right _ -> True

instance Show SomeTypeRep where
    show (SomeTypeRep a) = ("Some " ++) . show' a $ ""

show' :: TypeRep a -> (String -> String)
show' a =
    case a of
        Char -> ("Char" ++)
        Var n -> ('a' :) . (show n ++)
        _ -> ("Char :->: Char" ++)

nestedShow :: TypeRep a -> (String -> String)
nestedShow a =
    case a of
        (_ :->: _) -> ('(' :) . show' a . (')' :)
        _ -> show' a

unify :: TypeRep a -> TypeRep b -> TypeMap -> Either String TypeMap
unify Char Char m = Right m
unify (Char :->: Char) (Char :->: Char) m = Right m
unify (Var n0) (Var n1) m = Right (Map.insert (max n0 n1) (SomeTypeRep (Var (min n0 n1))) m)
unify (Var n) t m = Right (Map.insert n (SomeTypeRep t) m)
unify t (Var n) m = Right (Map.insert n (SomeTypeRep t) m)
unify a b _ = Left (("cannot unify " ++) . nestedShow a . (" against " ++) . nestedShow b $ "")
