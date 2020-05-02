module TestSkully.Stdlib.Containers.Pair (
    testSkullyPair
) where

import Prelude hiding (fst, snd)

import Test.Hspec
import Helpers

import Skully.Base
import Skully.Stdlib.Combinators
import Skully.Stdlib.Containers.Pair

testWithPair :: Spec
testWithPair =
    describe "withPair :: Skully (Pair r a b -> (a -> b -> r) -> r)" $ do
        it "should apply its elements to its callback" $
            withStreamsShouldReturn ("", "") (s, ("", "x")) $
                withPair .$ (pair .$ char 'x' .$ u) .$ (c .$ (c .$ (a .$ s)) .$ a)
        it "should apply its elements to its callback - different arguments" $
            withStreamsShouldReturn ("", "") (y, ("", "")) $
                withPair .$ (pair .$ (k .$ y) .$ k) .$ i

testFst :: Spec
testFst =
    describe "fst :: Skully (Pair a a b -> a)" $ do
        it "should get the first element of the pair" $
            withStreamsShouldReturn ("", "") (char 'x', ("", "")) $
                fst .$ (pair .$ char 'x' .$ u)
        it "should get the first element of the pair - different arguments" $
            withStreamsShouldReturn ("", "") (y, ("", "")) $
                fst .$ (pair .$ y .$ s)

testSnd :: Spec
testSnd =
    describe "snd :: Skully (Pair b a b -> a)" $ do
        it "should get the second element of the pair" $
            withStreamsShouldReturn ("", "") (u, ("", "")) $
                snd .$ (pair .$ char 'x' .$ u)
        it "should get the second element of the pair - different arguments" $
            withStreamsShouldReturn ("", "") (s, ("", "")) $
                snd .$ (pair .$ y .$ s)

testSwap :: Spec
testSwap =
    describe "swap :: Skully (Pair (Pair r b a) a b -> Pair r b a" $ do
        it "should swap the pair entries" $
            withStreamsShouldReturn ("", "") (u, ("", "")) $
                fst .$ (swap .$ (pair .$ char 'x' .$ u))
        it "should swap the pair entries - different arguments" $
            withStreamsShouldReturn ("", "") (y, ("", "")) $
                snd .$ (swap .$ (pair .$ y .$ s))

testSkullyPair :: Spec
testSkullyPair =
    describe "Skully.Stdlib.Containers.List" $ do
        testWithPair
        testFst
        testSnd
        testSwap
