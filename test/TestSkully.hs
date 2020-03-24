module TestSkully (
    testSkully
) where

import Test.Hspec

import Skully

testShowSkully :: Spec
testShowSkully = describe "show :: Skully a -> String" $ do
    it "prints S as \"s\"" $ show S `shouldBe` "s"
    it "prints K as \"k\"" $ show K `shouldBe` "k"

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully