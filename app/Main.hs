module Main where

import Prelude hiding (reverse)

import Data.Functor (void)
import Skully.Stdlib

c2 :: Skully ((b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c)
c2 = c .$ c .$ c

ifC :: Skully (Char -> Char -> a -> a -> a)
ifC = c2 .$ (c .$ d) .$ (c2 .$ f .$ e)

readLine :: Skully (List b Char)
readLine = y .$ (c .$ l .$ (c .$ (s .$ (f .$ (f .$ ifC .$ (char '\n')) .$ nil)) .$ (f .$ cons)))

readLines :: Skully a
readLines = y .$ (c .$ (f .$ (withList .$ (reverse .$ readLine)) .$ u) .$ (u .$ (char '\n')))

main :: IO ()
main = do
    let original = readLines
    putStrLn "--- original"
    print original

    let optimized = optimize original
    putStrLn "--- optimized"
    print optimized

    putStrLn "--- execution (input lines as necessary)"
    void (eval optimized)
