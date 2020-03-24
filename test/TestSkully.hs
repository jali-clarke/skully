{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestSkully (
    testSkully
) where

import Test.Hspec
import Control.Monad.State.Lazy

import CharSocket
import Skully

newtype FakeCharSocket a = FakeCharSocket (StateT (String, String) Maybe a)
    deriving (Functor, Applicative, Monad)

runFakeCharSocket :: (String, String) -> FakeCharSocket a -> Maybe (a, (String, String))
runFakeCharSocket streams (FakeCharSocket action) = runStateT action streams

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
    it "prints Ap S S as \"s s\"" $ show (Ap S S) `shouldBe` "s s"
    it "prints Ap S Q as \"s q\"" $ show (Ap S Q) `shouldBe` "s q"
    it "prints Ap K Q as \"k q\"" $ show (Ap K Q) `shouldBe` "k q"
    it "prints Ap S (Ap K K) as \"s (k k)\"" $ show (Ap S (Ap K K)) `shouldBe` "s (k k)"
    it "prints Ap (Ap S K) K as \"s k k\"" $ show (Ap (Ap S K) K) `shouldBe` "s k k"

testEvalSkully :: Spec
testEvalSkully = describe "eval :: CharSocket m => Skully a -> m a" $ do
    it "eval S abc ab a = abc a (ab a)" $
        let evaluated = runFakeCharSocket ("", "") $ do
                action <- eval S
                pure $ action (\a b -> show a ++ " " ++ show b) (* 2) (1 :: Int)
        in evaluated `shouldBe` Just ("1 2", ("", ""))
    it "eval K a b = a" $
        let evaluated = runFakeCharSocket ("", "") $ do
                action <- eval K
                pure $ action 'a' 'b'
        in evaluated `shouldBe` Just ('a', ("", ""))

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully
    testEvalSkully