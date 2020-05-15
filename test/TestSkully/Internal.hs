module TestSkully.Internal (
    testSkullyInternal
) where

import Test.Hspec

import TestSkully.Internal.TypeRep (testSkullyTypeRep)

testSkullyInternal :: Spec
testSkullyInternal =
    describe "Skully.Internal" $ do
        testSkullyTypeRep
