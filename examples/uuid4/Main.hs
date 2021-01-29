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

addChar :: Skully (Char -> Char -> Char)
addChar = y .$ (c .$ (s .$ (c .$ s .$ (c .$ d .$ (f .$ e .$ char '\x00')))) .$ (c .$ (c .$ (f .$ q)) .$ (c .$ (c .$ k) .$ (f .$ (c .$ c .$ (c .$ (f .$ c) .$ (c .$ (c .$ (c .$ k)) .$ f))) .$ q))))

adjust :: Int -> Skully (Char -> Char)
adjust n =
    case n `compare` 0 of
        EQ -> i
        LT -> f .$ q .$ (c .$ k .$ (adjust (n + 1)))
        GT -> addChar .$ char (chr n)

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

printPairAsHex :: Skully (Pair b Char Char -> b -> b)
printPairAsHex = c .$ (f .$ c .$ (f .$ (c .$ c .$ (c .$ (f .$ c) .$ (f .$ u))) .$ u)) .$ withPair

printByteAsHex :: Skully (Char -> a -> a)
printByteAsHex = c .$ printPairAsHex .$ charToHex

getAndPrintBytesAsHex :: Int -> Skully (a -> a)
getAndPrintBytesAsHex n =
    if n < 1
        then i
        else l .$ i .$ (f .$ printByteAsHex .$ (getAndPrintBytesAsHex (n - 1)))

printDash :: Skully (a -> a)
printDash = u .$ (char '-')

timeLow :: Skully (a -> a)
timeLow = getAndPrintBytesAsHex 4

timeMid :: Skully (a -> a)
timeMid = getAndPrintBytesAsHex 2

timeHiAndVersion :: Skully (a -> a)
timeHiAndVersion = s .$ l .$ (f .$ (c .$ c .$ (c .$ (f .$ printPairAsHex) .$ getAndPrintBytesAsHex 1)) .$ (c .$ (f .$ withPair .$ (k .$ (pair .$ char '4'))) .$ charToHex))

clkAll :: Skully (a -> a)
clkAll = getAndPrintBytesAsHex 2

node :: Skully (a -> a)
node = getAndPrintBytesAsHex 6

genUUID4 :: Skully (a -> a)
genUUID4 = timeLow .$ (printDash .$ (timeMid .$ (printDash .$ (timeHiAndVersion .$ (printDash .$ (clkAll .$ (printDash .$ (node .$ i))))))))

main :: IO ()
main = do
    let original = genUUID4
    putStrLn "--- original"
    print original

    let optimized = optimize original
    putStrLn "--- optimized"
    print optimized

    putStrLn "--- execution (input lines as necessary)"
    void (eval optimized)
