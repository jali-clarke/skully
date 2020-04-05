module Skully.CharSocket (
    CharSocket(..)
) where

import Prelude as P

class Monad m => CharSocket m where
    getChar :: m Char
    putChar :: Char -> m ()

instance CharSocket P.IO where
    getChar = P.getChar
    putChar = P.putChar