module Skully (
    Skully(..)
) where

data Skully a = S | K | U

instance Show (Skully a) where
    show skully =
        case skully of
            S -> "s"
            K -> "k"
            U -> "u"