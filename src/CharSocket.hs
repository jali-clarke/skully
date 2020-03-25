{-# LANGUAGE RankNTypes #-}

module CharSocket (
    CharSocket(..),
    AnyCharSocket(..)
) where

import Prelude hiding (getChar, putChar)

class Monad m => CharSocket m where
    getChar :: m Char
    putChar :: Char -> m ()

newtype AnyCharSocket a = AnyCharSocket (forall m. CharSocket m => m a)

instance Functor AnyCharSocket where
    fmap f (AnyCharSocket action) = AnyCharSocket (fmap f action)

instance Applicative AnyCharSocket where
    pure a = AnyCharSocket (pure a)
    AnyCharSocket actionF <*> AnyCharSocket actionA = AnyCharSocket (actionF <*> actionA)

instance Monad AnyCharSocket where
    AnyCharSocket action >>= g =
        let runAnyCharSocket (AnyCharSocket action') = action'
        in AnyCharSocket $ action >>= (runAnyCharSocket . g)

instance CharSocket AnyCharSocket where
    getChar = AnyCharSocket getChar
    putChar c = AnyCharSocket (putChar c)