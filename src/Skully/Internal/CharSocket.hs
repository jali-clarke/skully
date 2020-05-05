module Skully.Internal.CharSocket (
    CharSocket(..)
) where

import Prelude as P
import qualified System.IO as P

class Monad m => CharSocket m where
    getChar :: m (Maybe Char)
    putChar :: Char -> m ()

instance CharSocket P.IO where
    getChar = do
        eof <- P.isEOF
        if eof
            then pure Nothing
            else fmap Just P.getChar

    putChar c = do
        P.putChar c
        P.hFlush P.stdout
