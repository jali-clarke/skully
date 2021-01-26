import Data.Char (chr)
import Data.Functor (void)
import Prelude hiding (divMod, fst, snd)

import Skully.Stdlib

type SBool b = b -> b -> b

true :: Skully (SBool b)
true = k

false :: Skully (SBool b)
false = f .$ k

if_ :: Skully (SBool b -> b -> b -> b)
if_ = i

adjust :: Int -> Skully (Char -> Char)
adjust n =
    case n `compare` 0 of
        EQ -> i
        LT -> f .$ q .$ (c .$ k .$ (adjust (n + 1)))
        GT -> f .$ q .$ (k .$ (adjust (n - 1)))

gt :: Int -> Skully (Char -> SBool b)
gt n = f .$ (f .$ (f .$ (f .$ e .$ (char (chr n))) .$ false) .$ false) .$ true

overD2 :: Skully ((a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c)
overD2 = c .$ (c .$ s) .$ c

overD3 :: Skully ((a -> b -> c -> d) -> (x -> a) -> (x -> b) -> (x -> c) -> x -> d)
overD3 = c .$ (c .$ (c .$ s)) .$ overD2

divMod' :: Int -> Skully (Char -> Char -> Pair b Char Char)
divMod' n = y .$ (f .$ (c .$ (overD2 .$ (overD3 .$ if_ .$ (gt (n - 1)))) .$ (c .$ (c .$ (f .$ c .$ (adjust (negate n)))) .$ (f .$ c .$ (adjust 1)))) .$ pair)

divMod :: Int -> Skully (Char -> Pair b Char Char)
divMod n = divMod' n .$ (char '\x00')

decHexDigitToHex :: Skully (Char -> Char)
decHexDigitToHex = overD3 .$ if_ .$ (gt 9) .$ (adjust 87) .$ (adjust 48)

charToHex :: Skully (Char -> Pair b Char Char)
charToHex = c .$ (f .$ withPair .$ (overD2 .$ c .$ (f .$ c) .$ (c .$ pair) .$ decHexDigitToHex)) .$ (divMod 16)

printByteAsHex :: Skully (Char -> a -> a)
printByteAsHex = c .$ (f .$ c .$ (c .$ (f .$ c .$ u) .$ (c .$ (f .$ c) .$ (f .$ u)))) .$ (c .$ withPair .$ charToHex)

getAndPrintBytesAsHex :: Int -> Skully (a -> a)
getAndPrintBytesAsHex n =
    if n < 1
        then i
        else l .$ i .$ (f .$ printByteAsHex .$ (getAndPrintBytesAsHex (n - 1)))

printDash :: Skully (a -> a)
printDash = u .$ (char '-')

dashPrefixed :: (b -> Skully (a -> a)) -> b -> Skully (a -> a)
dashPrefixed cont b = printDash .$ cont b

genUUID4 :: Skully (a -> a)
genUUID4 = getAndPrintBytesAsHex 4 .$ (dashPrefixed getAndPrintBytesAsHex 2 .$ (dashPrefixed getAndPrintBytesAsHex 2 .$ (dashPrefixed getAndPrintBytesAsHex 2 .$ (dashPrefixed getAndPrintBytesAsHex 6 .$ i))))

main :: IO ()
main = void $ eval (optimize genUUID4)
