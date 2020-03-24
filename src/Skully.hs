module Skully (
    Skully(..)
) where

data Skully a = S | K | U | L | Y | Q | Ap (Skully a) (Skully a)

instance Show (Skully a) where
    show skully =
        case skully of
            S -> "s"
            K -> "k"
            U -> "u"
            L -> "l"
            Y -> "y"
            Q -> "q"
            Ap _ _ -> "s s"