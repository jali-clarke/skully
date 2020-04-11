module TestSkully.Stdlib (
    testSkullyStdlib
) where

import Test.Hspec

import TestSkully.Stdlib.Combinators (testSkullyCombinators)

testSkullyStdlib :: Spec
testSkullyStdlib =
    describe "Skully.Stdlib" $ do
        testSkullyCombinators
