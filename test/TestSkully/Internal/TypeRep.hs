module TestSkully.Internal.TypeRep (
    testSkullyTypeRep
) where

import Test.Hspec

import qualified Data.IntMap.Strict as Map
import Skully.Internal.TypeRep

testUnify :: Spec
testUnify =
    describe "unify :: TypeRep a -> TypeRep b -> TypeMap -> Either String TypeMap" $ do
        it "should unify Char against Char" $ unify Char Char Map.empty `shouldBe` Right Map.empty
        it "should not unify (Char :->: Char) against Char" $
            unify (Char :->: Char) Char Map.empty `shouldBe` Left "cannot unify (Char :->: Char) against Char"
        it "should unify (Char :->: Char) against (Char :->: Char)" $
            unify (Char :->: Char) (Char :->: Char) Map.empty `shouldBe` Right Map.empty
        it "should not unify Char against (Char :->: Char)" $
            unify Char (Char :->: Char) Map.empty `shouldBe` Left "cannot unify Char against (Char :->: Char)"
        it "should unify (Var 0) against Char as (Var 0) ~ Char" $
            unify (Var 0) Char Map.empty `shouldBe` Right (Map.insert 0 (SomeTypeRep Char) Map.empty)
        it "should unify Char against (Var 0) as (Var 0) ~ Char" $
            unify Char (Var 0) Map.empty `shouldBe` Right (Map.insert 0 (SomeTypeRep Char) Map.empty)
        it "should unify (Var 0) against (Var 1) as (Var 1) ~ (Var 0)" $
            unify (Var 0) (Var 1) Map.empty `shouldBe` Right (Map.insert 1 (SomeTypeRep (Var 0)) Map.empty)
        it "should unify (Var 4) against (Var 2) as (Var 4) ~ (Var 2)" $
            unify (Var 4) (Var 2) Map.empty `shouldBe` Right (Map.insert 4 (SomeTypeRep (Var 2)) Map.empty)
        it "should unify (Var 66) against Char as (Var 66) ~ Char" $
            unify (Var 66) Char Map.empty `shouldBe` Right (Map.insert 66 (SomeTypeRep Char) Map.empty)
        it "should unify Char against (Var 78) as (Var 78) ~ Char" $
            unify Char (Var 78) Map.empty `shouldBe` Right (Map.insert 78 (SomeTypeRep Char) Map.empty)
        it "should unify (Var 66) against (Char :->: Char) as (Var 66) ~ (Char :->: Char)" $
            unify (Var 66) (Char :->: Char) Map.empty `shouldBe` Right (Map.insert 66 (SomeTypeRep (Char :->: Char)) Map.empty)
        it "should unify (Char :->: Char) against (Var 78) as (Var 78) ~ (Char :->: Char)" $
            unify (Char :->: Char) (Var 78) Map.empty `shouldBe` Right (Map.insert 78 (SomeTypeRep (Char :->: Char)) Map.empty)
        it "should unify Char against Char with existing constraint" $
            unify Char Char (Map.insert 3 (SomeTypeRep Char) Map.empty) `shouldBe` Right (Map.insert 3 (SomeTypeRep Char) Map.empty)
        it "should unify (Char :->: Char) against (Char :->: Char) with existing constraint" $
            unify (Char :->: Char) (Char :->: Char) (Map.insert 3 (SomeTypeRep Char) Map.empty) `shouldBe` Right (Map.insert 3 (SomeTypeRep Char) Map.empty)
        it "should unify (Var 0) against (Var 1) as (Var 1) ~ (Var 0) with unrelated constraint" $
            unify (Var 0) (Var 1) (Map.insert 2 (SomeTypeRep Char) Map.empty) `shouldBe` Right (Map.insert 1 (SomeTypeRep (Var 0)) (Map.insert 2 (SomeTypeRep Char) Map.empty))
        it "should unify Char against (Var 78) as (Var 78) ~ Char with unrelated constraint" $
            unify Char (Var 78) (Map.insert 0 (SomeTypeRep Char) Map.empty) `shouldBe` Right (Map.insert 0 (SomeTypeRep Char) (Map.insert 78 (SomeTypeRep Char) Map.empty))
        it "should unify (Var 66) against (Char :->: Char) as (Var 66) ~ (Char :->: Char) with unrelated constraint" $
            unify (Var 66) (Char :->: Char) (Map.insert 0 (SomeTypeRep Char) Map.empty)`shouldBe` Right (Map.insert 0 (SomeTypeRep Char) (Map.insert 66 (SomeTypeRep (Char :->: Char)) Map.empty))

testSkullyTypeRep :: Spec
testSkullyTypeRep =
    describe "Skully.Internal.TypeRep" $ do
        testUnify
