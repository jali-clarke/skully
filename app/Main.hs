module Main where

import Prelude hiding (reverse)

import Skully.Stdlib

c2 :: Skully ((b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c)
c2 = c .$ c .$ c

ifC :: Skully (Char -> Char -> a -> a -> a)
ifC = c2 .$ (c .$ d) .$ (c2 .$ f .$ e)

readLine :: Skully (List b Char)
readLine = y .$ (optimize $ c .$ l .$ (c .$ (s .$ (f .$ (f .$ ifC .$ (char '\n')) .$ nil)) .$ (f .$ cons)))

readLines :: Skully a
readLines = y .$ (optimize $ c .$ (f .$ (withList .$ (reverse .$ readLine)) .$ u) .$ (u .$ (char '\n')))

main :: IO ()
main = print readLines <* eval readLines
