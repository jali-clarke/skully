module CharSocket (
    CharSocket(..)
) where

import Prelude hiding (getChar, putChar)

class Monad m => CharSocket m where
    getChar :: m Char
    putChar :: Char -> m ()
