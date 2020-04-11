module TestSkully.Stdlib.Containers (
    testSkullyContainers
) where

import Test.Hspec

import TestSkully.Stdlib.Containers.List (testSkullyList)

testSkullyContainers :: Spec
testSkullyContainers =
    describe "Skully.Stdlib.Containers" $ do
        testSkullyList