module TestSK (
    testSK
) where

import Test.Hspec

import SK

testShowSK :: Spec
testShowSK = describe "show :: SK a -> String" $ do
    it "prints S as \"S\"" $ show S `shouldBe` "s"

testSK :: Spec
testSK = describe "operations on SK a" $ do
    testShowSK