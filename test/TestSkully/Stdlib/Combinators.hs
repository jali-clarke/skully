module TestSkully.Stdlib.Combinators (
    testSkullyCombinators
) where

import Test.Hspec
import Helpers

import Skully.Base
import Skully.Stdlib.Combinators

testEvalI :: Spec
testEvalI =
    describe "eval-ing i exprs" $ do
        it "returns its argument in i .$ u" $
            withStreamsShouldReturn ("", "") ("u", ("", "")) $
                i .$ u
        it "evaluates its argument in i .$ (k .$ char 'x' .$ s)" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                i .$ (k .$ char 'x' .$ s)

testEvalSkully :: Spec
testEvalSkully =
    describe "eval :: CharSocket m => Skully a -> m (Skully a)" $ do
        testEvalI

testSkullyCombinators :: Spec
testSkullyCombinators =
    describe "Skully.Stdlib.Combinators" $ do
        testEvalSkully
