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
        it "should unify (Var 0) against Char with (Var 0) ~ Char" $
            unify (Var 0) Char Map.empty `shouldBe` Right (Map.insert 0 (SomeTypeRep Char) Map.empty)
        it "should unify Char against (Var 0) with (Var 0) ~ Char" $
            unify Char (Var 0) Map.empty `shouldBe` Right (Map.insert 0 (SomeTypeRep Char) Map.empty)
        it "should unify (Var 0) against (Var 1) with (Var 1) ~ (Var 0)" $
            unify (Var 0) (Var 1) Map.empty `shouldBe` Right (Map.insert 1 (SomeTypeRep (Var 0)) Map.empty)
        it "should unify (Var 4) against (Var 2) with (Var 4) ~ (Var 2)" $
            unify (Var 4) (Var 2) Map.empty `shouldBe` Right (Map.insert 4 (SomeTypeRep (Var 2)) Map.empty)
        it "should unify (Var 66) against Char with (Var 66) ~ Char" $
            unify (Var 66) Char Map.empty `shouldBe` Right (Map.insert 66 (SomeTypeRep Char) Map.empty)
        it "should unify Char against (Var 78) with (Var 78) ~ Char" $
            unify Char (Var 78) Map.empty `shouldBe` Right (Map.insert 78 (SomeTypeRep Char) Map.empty)

testSkullyTypeRep :: Spec
testSkullyTypeRep =
    describe "Skully.Internal.TypeRep" $ do
        testUnify
