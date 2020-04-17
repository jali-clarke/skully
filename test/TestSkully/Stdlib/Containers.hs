module TestSkully.Stdlib.Containers (
    testSkullyContainers
) where

import Test.Hspec

import TestSkully.Stdlib.Containers.List (testSkullyList)
import TestSkully.Stdlib.Containers.Pair (testSkullyPair)

testSkullyContainers :: Spec
testSkullyContainers =
    describe "Skully.Stdlib.Containers" $ do
        testSkullyList
        testSkullyPair
