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
            withStreamsShouldReturn ("", "") ("'u'", ("", "")) $
                a .$ char 'u' .$ k .$ l
        it "evaluates its result in a .$ s .$ (u .$ char 'j')" $
            withStreamsShouldReturn ("", "") ("s", ("", "j")) $
                a .$ s .$ (u .$ char 'j')

testEvalI :: Spec
testEvalI =
    describe "eval-ing i exprs" $ do
        it "returns its argument in i .$ u" $
            withStreamsShouldReturn ("", "") ("u", ("", "")) $
                i .$ u
        it "evaluates its result in i .$ (k .$ char 'x' .$ s)" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                i .$ (k .$ char 'x' .$ s)

testEvalSkully :: Spec
testEvalSkully =
    describe "eval :: CharSocket m => Skully a -> m (Skully a)" $ do
        testEvalA
        testEvalI

testSkullyCombinators :: Spec
testSkullyCombinators =
    describe "Skully.Stdlib.Combinators" $ do
        testEvalSkully
