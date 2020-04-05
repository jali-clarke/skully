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

runWithStreams :: (String, String) -> FakeCharSocket a -> Maybe (a, (String, String))
runWithStreams streams (FakeCharSocket action) = runStateT action streams

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
    it "prints E as \"e\"" $ show E `shouldBe` "e"
    it "prints Ap S S as \"ss\"" $ show (Ap S S) `shouldBe` "ss"
    it "prints Ap S Q as \"sq\"" $ show (Ap S Q) `shouldBe` "sq"
    it "prints Ap K Q as \"kq\"" $ show (Ap K Q) `shouldBe` "kq"
    it "prints Ap S (Ap K K) as \"s(kk)\"" $ show (Ap S (Ap K K)) `shouldBe` "s(kk)"
    it "prints Ap (Ap S K) K as \"skk\"" $ show (Ap (Ap S K) K) `shouldBe` "skk"
    it "prints Char 'x' as \"'x'\"" $ show (Char 'x') `shouldBe` "'x'"
    it "prints Char 'y' as \"'y'\"" $ show (Char 'y') `shouldBe` "'y'"

testEvalSkully :: Spec
testEvalSkully = describe "eval :: CharSocket m => Skully a -> m (Skully a)" $
    let eval' = fmap show . eval
        withStreamsShouldReturn initStreams expectedResult expr = runWithStreams initStreams (eval' expr) `shouldBe` Just expectedResult
    in do
        it "is a no-op when evaluating Char 'x'" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                Char 'x'
        it "is a no-op when evaluating S" $
            withStreamsShouldReturn ("", "") ("s", ("", "")) $
                S
        it "outputs 'x' when evaluating Ap (Ap U (Char 'x')) K" $
            withStreamsShouldReturn ("", "") ("k", ("", "x")) $
                Ap (Ap U (Char 'x')) K
        it "outputs 'x' when evaluating Ap (Ap U (Char 'y')) K" $
            withStreamsShouldReturn ("", "") ("k", ("", "y")) $
                Ap (Ap U (Char 'y')) K
        it "outputs 'x' when evaluating Ap (Ap U (Char 'y')) S" $
            withStreamsShouldReturn ("", "") ("s", ("", "y")) $
                Ap (Ap U (Char 'y')) S
        it "outputs 'x' and then 'y' when evaluating Ap (Ap U (Char 'x')) (Ap (Ap U (Char 'y')) L)" $
            withStreamsShouldReturn ("", "") ("l", ("", "xy")) $
                Ap (Ap U (Char 'x')) (Ap (Ap U (Char 'y')) L)

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully
    testEvalSkully