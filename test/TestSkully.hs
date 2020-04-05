{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestSkully (
    testSkully
) where

import Prelude hiding (getChar, putChar)

import Test.Hspec
import Control.Monad.State.Lazy

import Skully
import Skully.CharSocket

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
    it "prints s as \"s\"" $ show s `shouldBe` "s"
    it "prints k as \"k\"" $ show k `shouldBe` "k"
    it "prints u as \"u\"" $ show u `shouldBe` "u"
    it "prints l as \"l\"" $ show l `shouldBe` "l"
    it "prints y as \"y\"" $ show y `shouldBe` "y"
    it "prints q as \"q\"" $ show q `shouldBe` "q"
    it "prints e as \"e\"" $ show e `shouldBe` "e"
    it "prints s .$ s as \"ss\"" $ show (s .$ s) `shouldBe` "ss"
    it "prints s .$ q as \"sq\"" $ show (s .$ q) `shouldBe` "sq"
    it "prints k .$ q as \"kq\"" $ show (k .$ q) `shouldBe` "kq"
    it "prints s .$ (k .$ k) as \"s(kk)\"" $ show (s .$ (k .$ k)) `shouldBe` "s(kk)"
    it "prints s .$ k .$ k as \"skk\"" $ show (s .$ k .$ k) `shouldBe` "skk"
    it "prints char 'x' as \"'x'\"" $ show (char 'x') `shouldBe` "'x'"
    it "prints char 'y' as \"'y'\"" $ show (char 'y') `shouldBe` "'y'"

testEvalSkully :: Spec
testEvalSkully = describe "eval :: CharSocket m => Skully a -> m (Skully a)" $
    let eval' = fmap show . eval
        withStreamsShouldReturn initStreams expectedResult expr = runWithStreams initStreams (eval' expr) `shouldBe` Just expectedResult
    in do
        it "is a no-op when evaluating char 'x'" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                char 'x'
        it "is a no-op when evaluating s" $
            withStreamsShouldReturn ("", "") ("s", ("", "")) $
                s
        it "outputs 'x' when evaluating u .$ char 'x' .$ k" $
            withStreamsShouldReturn ("", "") ("k", ("", "x")) $
                u .$ char 'x' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ k" $
            withStreamsShouldReturn ("", "") ("k", ("", "y")) $
                u .$ char 'y' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ s" $
            withStreamsShouldReturn ("", "") ("s", ("", "y")) $
                u .$ char 'y' .$ s
        it "outputs 'x' and then 'y' when evaluating u .$ char 'x' .$ (u .$ char 'y' .$ l)" $
            withStreamsShouldReturn ("", "") ("l", ("", "xy")) $
                u .$ char 'x' .$ (u .$ char 'y' .$ l)
        it "captures the first char from stdin and injects it when evaluating l .$ k" $
            withStreamsShouldReturn ("x", "") ("k'x'", ("", "")) $
                l .$ k
        it "captures the first (different) char from stdin and injects it when evaluating l .$ u" $
            withStreamsShouldReturn ("y", "") ("u'y'", ("", "")) $
                l .$ u
        it "returns the first argument when evaluating k .$ s .$ k" $
            withStreamsShouldReturn ("", "") ("s", ("", "")) $
                k .$ s .$ k
        it "returns the first argument when evaluating k .$ u .$ (u .$ char 'y' .$ q)" $
            withStreamsShouldReturn ("", "") ("u", ("", "")) $
                k .$ u .$ (u .$ char 'y' .$ q)
        it "evaluates the first argument when evaluating k .$ (u .$ char 'x' .$ k) .$ s" $
            withStreamsShouldReturn ("", "") ("k", ("", "x")) $
                k .$ (u .$ char 'x' .$ k) .$ s

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully
    testEvalSkully