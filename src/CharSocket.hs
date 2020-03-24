module CharSocket (
    CharSocket(..)
) where

class Monad m => CharSocket m where
    getChar :: m Char
    putChar :: Char -> m ()