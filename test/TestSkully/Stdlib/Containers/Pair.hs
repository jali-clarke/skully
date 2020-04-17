module TestSkully.Stdlib.Containers.Pair (
    testSkullyPair
) where

import Test.Hspec
import Helpers

import Skully.Base
import Skully.Stdlib.Combinators
import Skully.Stdlib.Containers.Pair

testWithPair :: Spec
testWithPair =
    describe "withPair :: Skully (Pair r a b -> (a -> b -> r) -> r)" $ do
        it "should apply its elements to its callback" $
            withStreamsShouldReturn ("", "") ("s", ("", "x")) $
                withPair .$ (pair .$ char 'x' .$ u) .$ (c .$ (c .$ (a .$ s)) .$ a)
        it "should apply its elements to its callback - different arguments" $
            withStreamsShouldReturn ("", "") ("y", ("", "")) $
                withPair .$ (pair .$ (k .$ y) .$ k) .$ i

testSkullyPair :: Spec
testSkullyPair =
    describe "Skully.Stdlib.Containers.List" $
        testWithPair