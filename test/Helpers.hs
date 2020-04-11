module Helpers (
    skip
) where

import Test.Hspec

skip :: (HasCallStack, Example a) => String -> (String -> a -> SpecWith (Arg a)) -> String -> a -> SpecWith ()
skip pendingReason _ description _ = it description $ pendingWith pendingReason
