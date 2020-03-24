module SK (
    SK(..)
) where

data SK a = S

instance Show (SK a) where
    show _ = "s"