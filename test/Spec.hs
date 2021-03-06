import Test.Hspec

import TestSkully.Base (testSkullyBase)
import TestSkully.Internal (testSkullyInternal)
import TestSkully.Stdlib (testSkullyStdlib)

main :: IO ()
main = hspec $ do
    describe "Skully" $ do
        testSkullyBase
        testSkullyInternal
        testSkullyStdlib
