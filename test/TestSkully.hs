module TestSkully (
    testSkully
) where

import Test.Hspec

import Skully

testShowSkully :: Spec
testShowSkully = describe "show :: Skully a -> String" $ do
    it "prints S as \"s\"" $ show S `shouldBe` "s"
    it "prints K as \"k\"" $ show K `shouldBe` "k"
    it "prints U as \"u\"" $ show U `shouldBe` "u"
    it "prints L as \"l\"" $ show L `shouldBe` "l"
    it "prints Y as \"y\"" $ show Y `shouldBe` "y"
    it "prints Q as \"q\"" $ show Q `shouldBe` "q"
    it "prints Ap S S as \"s s\"" $ show (Ap S S) `shouldBe` "s s"
    it "prints Ap S Q as \"s q\"" $ show (Ap S Q) `shouldBe` "s q"
    it "prints Ap K Q as \"k q\"" $ show (Ap K Q) `shouldBe` "k q"
    it "prints Ap S (Ap K K) as \"s (k k)\"" $ show (Ap S (Ap K K)) `shouldBe` "s (k k)"
    it "prints Ap (Ap S K) K as \"s k k\"" $ show (Ap (Ap S K) K) `shouldBe` "s k k"

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully