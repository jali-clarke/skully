module TestSkully.Stdlib.Combinators (
    testSkullyCombinators
) where

import Test.Hspec
import Helpers

import Skully.Base
import Skully.Stdlib.Combinators

testEvalA :: Spec
testEvalA =
    describe "eval-ing a exprs" $ do
        it "applies its second argument to its first in a .$ char 'u' .$ k .$ l" $
            withStreamsShouldReturn ("", "") (char 'u', ("", "")) $
                a .$ char 'u' .$ k .$ l
        it "evaluates its result in a .$ s .$ (u .$ char 'j')" $
            withStreamsShouldReturn ("", "") (s, ("", "j")) $
                a .$ s .$ (u .$ char 'j')

testEvalC :: Spec
testEvalC =
    describe "eval-ing c exprs" $ do
        it "composes its callbacks and evaluates the result in c .$ (u .$ char 'h') .$ (u .$ char 'i') .$ k" $
            withStreamsShouldReturn ("", "") (k, ("", "hi")) $
                c .$ (u .$ char 'h') .$ (u .$ char 'i') .$ k

testEvalD :: Spec
testEvalD =
    describe "eval-ing d exprs" $ do
        it "applies its second argument to its first twice in d .$ k .$ s" $
            withStreamsShouldReturn ("", "") (s, ("", "")) $
                d .$ k .$ s
        it "evaluates its result in d .$ u .$ char 'p'" $
            withStreamsShouldReturn ("", "") (char 'p', ("", "p")) $
                d .$ u .$ char 'p'

testEvalF :: Spec
testEvalF =
    describe "eval-ing f exprs" $ do
        it "swaps the arguments to its callback and evaluates the result in f .$ u .$ char 'x' .$ (i .$ char 'y')" $
            withStreamsShouldReturn ("", "") (char 'x', ("", "y")) $
                f .$ u .$ char 'x' .$ (i .$ char 'y')

testEvalI :: Spec
testEvalI =
    describe "eval-ing i exprs" $ do
        it "returns its argument in i .$ u" $
            withStreamsShouldReturn ("", "") (u, ("", "")) $
                i .$ u
        it "evaluates its result in i .$ (k .$ char 'x' .$ s)" $
            withStreamsShouldReturn ("", "") (char 'x', ("", "")) $
                i .$ (k .$ char 'x' .$ s)

testEvalSkully :: Spec
testEvalSkully =
    describe "eval :: CharSocket m => Skully a -> m (Skully a)" $ do
        testEvalA
        testEvalC
        testEvalD
        testEvalF
        testEvalI

testSkullyCombinators :: Spec
testSkullyCombinators =
    describe "Skully.Stdlib.Combinators" $ do
        testEvalSkully
