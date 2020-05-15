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
    show (SomeTypeRep a) = show a

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
unify Char Char _ = Right Map.empty
unify (Char :->: Char) (Char :->: Char) _ = Right Map.empty
unify (Var n0) (Var n1) _ = Right (Map.insert (max n0 n1) (SomeTypeRep (Var (min n0 n1))) Map.empty)
unify (Var n) _ _ = Right (Map.insert n (SomeTypeRep Char) Map.empty)
unify _ (Var n) _ = Right (Map.insert n (SomeTypeRep Char) Map.empty)
unify a b _ = Left (("cannot unify " ++) . nestedShow a . (" against " ++) . nestedShow b $ "")
