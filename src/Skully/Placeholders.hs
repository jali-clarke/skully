module Skully.Placeholders (
    liftSkully,
    unliftSkully
) where

import Skully.Base

{-# WARNING liftSkully "liftSkully is being used - will blow up if called" #-}
liftSkully :: a -> Skully a
liftSkully = error "liftSkully called"

{-# WARNING unliftSkully "unliftSkully is being used - will blow up if called" #-}
unliftSkully :: Skully a -> a
unliftSkully = error "unliftSkully called"
