module Skully.Internal.CharSocket (
    CharSocket(..)
) where

import Prelude as P
import qualified System.IO as P

class Monad m => CharSocket m where
    getChar :: m Char
    putChar :: Char -> m ()

instance CharSocket P.IO where
    getChar = P.getChar

    putChar c = do
        P.putChar c
        P.hFlush P.stdout
