{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestSkully (
    testSkully
) where

import Prelude hiding (getChar, putChar)

import Test.Hspec
import Control.Monad.State.Lazy

import CharSocket
import Skully

newtype FakeCharSocket a = FakeCharSocket (StateT (String, String) Maybe a)
    deriving (Functor, Applicative, Monad)

instance CharSocket FakeCharSocket where
    getChar = FakeCharSocket $ do
        (input, output) <- get
        case input of
            [] -> lift Nothing
            c : rest -> put (rest, output) *> pure c

    putChar c = FakeCharSocket $ do
        (input, output) <- get
        put (input, output ++ [c])

testShowSkully :: Spec
testShowSkully = describe "show :: Skully a -> String" $ do
    it "prints S as \"s\"" $ show S `shouldBe` "s"
    it "prints K as \"k\"" $ show K `shouldBe` "k"
    it "prints U as \"u\"" $ show U `shouldBe` "u"
    it "prints L as \"l\"" $ show L `shouldBe` "l"
    it "prints Y as \"y\"" $ show Y `shouldBe` "y"
    it "prints Q as \"q\"" $ show Q `shouldBe` "q"
    it "prints Ap S S as \"s s\"" $ show (Ap S S) `shouldBe` "ss"
    it "prints Ap S Q as \"s q\"" $ show (Ap S Q) `shouldBe` "sq"
    it "prints Ap K Q as \"k q\"" $ show (Ap K Q) `shouldBe` "kq"
    it "prints Ap S (Ap K K) as \"s (k k)\"" $ show (Ap S (Ap K K)) `shouldBe` "s(kk)"
    it "prints Ap (Ap S K) K as \"s k k\"" $ show (Ap (Ap S K) K) `shouldBe` "skk"

testEvalSkully :: Spec
testEvalSkully = describe "eval :: CharSocket m => Skully a -> m ()" $ do
    pure ()
                
testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully
    testEvalSkully