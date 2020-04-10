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

withStreamsShouldReturn :: (String, String) -> (String, (String, String)) -> Skully a -> Expectation
withStreamsShouldReturn initStreams expectedResult expr =
    let eval' = fmap show . eval
    in runWithStreams initStreams (eval' expr) `shouldBe` Just expectedResult

testEvalSkullyChar :: Spec
testEvalSkullyChar = describe "eval-ing char expressions" $ do
    it "is a no-op when evaluating char 'x'" $
        withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
            char 'x'

testEvalSkullyS :: Spec
testEvalSkullyS = describe "eval-ing s expressions" $ do
    it "is a no-op when evaluating s" $
        withStreamsShouldReturn ("", "") ("s", ("", "")) $
            s
    it "uses its argument appropriately when evaluating s .$ k .$ k .$ y" $ do
        withStreamsShouldReturn ("", "") ("y", ("", "")) $
            s .$ k .$ k .$ y
    it "evaluates its result when evaluating s .$ u .$ k .$ char 'x'" $ do
        withStreamsShouldReturn ("", "") ("k'x'", ("", "x")) $
            s .$ u .$ k .$ char 'x'

testEvalSkullyK :: Spec
testEvalSkullyK = describe "eval-ing k expressions" $ do
    it "returns the first argument when evaluating k .$ s .$ k" $
        withStreamsShouldReturn ("", "") ("s", ("", "")) $
            k .$ s .$ k
    it "returns the first argument when evaluating k .$ u .$ (u .$ char 'y' .$ q)" $
        withStreamsShouldReturn ("", "") ("u", ("", "")) $
            k .$ u .$ (u .$ char 'y' .$ q)
    it "evaluates the first argument when evaluating k .$ (u .$ char 'x' .$ k) .$ s" $
        withStreamsShouldReturn ("", "") ("k", ("", "x")) $
            k .$ (u .$ char 'x' .$ k) .$ s

testEvalSkullyU :: Spec
testEvalSkullyU = describe "eval-ing u expressions" $ do
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
    it "evaluates its char arg to produce a char when evaluating u .$ (k .$ char 'x' .$ k) .$ s" $
        withStreamsShouldReturn ("", "") ("s", ("", "x")) $
            u .$ (k .$ char 'x' .$ k) .$ s

testEvalSkullyL :: Spec
testEvalSkullyL = describe "eval-ing l expressions" $ do
    it "captures the first char from stdin and injects it when evaluating l .$ k" $
        withStreamsShouldReturn ("x", "") ("k'x'", ("", "")) $
            l .$ k
    it "captures the first (different) char from stdin and injects it when evaluating l .$ u" $
        withStreamsShouldReturn ("y", "") ("u'y'", ("", "")) $
            l .$ u
    it "evaluates the resulting expression after capturing the char when evaluating l .$ (k .$ s)" $
        withStreamsShouldReturn ("a", "") ("s", ("", "")) $
            l .$ (k .$ s)

testEvalSkully :: Spec
testEvalSkully = describe "eval :: CharSocket m => Skully a -> m (Skully a)" $ do
    testEvalSkullyChar
    testEvalSkullyS
    testEvalSkullyK
    testEvalSkullyU
    testEvalSkullyL

testSkully :: Spec
testSkully = describe "operations on Skully a" $ do
    testShowSkully
    testEvalSkully