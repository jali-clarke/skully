module TestSkully.Stdlib (
    testSkullyStdlib
) where

import Test.Hspec

import TestSkully.Stdlib.Combinators (testSkullyCombinators)
import TestSkully.Stdlib.Containers (testSkullyContainers)

testSkullyStdlib :: Spec
testSkullyStdlib =
    describe "Skully.Stdlib" $ do
        testSkullyCombinators
        testSkullyContainers