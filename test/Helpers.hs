{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers (
    FakeCharSocket,
    runWithStreams,
    withStreamsShouldReturn,

    skip
) where

import Test.Hspec

import Control.Monad.State.Lazy

import Skully.Base
import Skully.Internal.CharSocket

newtype FakeCharSocket a = FakeCharSocket (State (String, String) a)
    deriving (Functor, Applicative, Monad)

instance CharSocket FakeCharSocket where
    initSocket = FakeCharSocket (pure ())

    getChar = FakeCharSocket $ do
        (input, output) <- get
        case input of
            [] -> pure Nothing
            c : rest -> put (rest, output) *> pure (Just c)

    putChar c = FakeCharSocket $ do
        (input, output) <- get
        put (input, output ++ [c])

runWithStreams :: (String, String) -> FakeCharSocket a -> (a, (String, String))
runWithStreams streams (FakeCharSocket action) = runState action streams

withStreamsShouldReturn :: (String, String) -> (Skully a, (String, String)) -> Skully a -> Expectation
withStreamsShouldReturn initStreams expectedResult expr =
    runWithStreams initStreams (eval expr) `shouldBe` expectedResult

skip :: (HasCallStack, Example a) => String -> (String -> a -> SpecWith (Arg a)) -> String -> a -> SpecWith ()
skip pendingReason _ description _ = it description $ pendingWith pendingReason
