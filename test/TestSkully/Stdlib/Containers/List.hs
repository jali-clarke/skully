module TestSkully.Stdlib.Containers.List (
    testSkullyList
) where

import Test.Hspec
import Helpers

import Skully.Base
import Skully.Stdlib.Containers.List

testSkullyListNil :: Spec
testSkullyListNil =
    describe "withList on nil" $ do
        it "returns its non-list argument in withList .$ nil .$ u .$ s" $
            withStreamsShouldReturn ("", "") ("u", ("", "")) $
                withList .$ nil .$ u .$ s

testSkullyListCons :: Spec
testSkullyListCons =
    describe "withList on non-empty lists" $ do
        it "prints all elements when given u in withList .$ (cons .$ char 'x' .$ (cons .$ char 'y' .$ nil)) .$ k .$ u" $
            withStreamsShouldReturn ("", "") ("k", ("", "xy")) $
                withList .$ (cons .$ char 'x' .$ (cons .$ char 'y' .$ nil)) .$ k .$ u

testSkullyList :: Spec
testSkullyList =
    describe "Skully.Stdlib.Containers.List" $ do
        describe "withList :: Skully (List b a -> b -> (a -> b -> b) -> b)" $ do
            testSkullyListNil
            testSkullyListCons
