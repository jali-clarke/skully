module TestSkully.Stdlib.Containers.Pair (
    testSkullyPair
) where

import Prelude hiding (fst)

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

testFst :: Spec
testFst =
    describe "fst :: Skully (Pair a a b -> a)" $ do
        it "should get the first element of the pair" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                fst .$ (pair .$ char 'x' .$ u)
        it "should get the first element of the pair - different arguments" $
            withStreamsShouldReturn ("", "") ("y", ("", "")) $
                fst .$ (pair .$ y .$ s)

testSkullyPair :: Spec
testSkullyPair =
    describe "Skully.Stdlib.Containers.List" $ do
        testWithPair
        testFst