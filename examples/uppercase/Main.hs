import Prelude hiding (Bool, and)

import Data.Functor (void)
import Skully.Stdlib

type Unit b = b -> b

unit :: Skully (Unit b)
unit = i

type Bool b = b -> b -> b

true :: Skully (Bool b)
true = k

false :: Skully (Bool b)
false = f .$ k

ifB :: Skully (Bool b -> b -> b -> b)
ifB = i

dec :: Skully (Char -> Char)
dec = f .$ q .$ k

sub :: Int -> Skully (Char -> Char)
sub n = if n < 1 then i else c .$ dec .$ sub (n - 1)

capitalize :: Skully (Char -> Char)
capitalize = sub 32

and :: Skully (Bool b -> Bool b -> Bool b)
and = c .$ (c .$ (c .$ d)) .$ (f .$ (c .$ c .$ (c .$ c .$ (c .$ c .$ ifB))) .$ ifB)

isGreaterThanBacktick :: Skully (Char -> Bool b)
isGreaterThanBacktick = f .$ (f .$ (f .$ (e .$ char '`') .$ true) .$ false) .$ false

isLessThanLeftBrace :: Skully (Char -> Bool b)
isLessThanLeftBrace = f .$ (f .$ (f .$ (e .$ char '{') .$ false) .$ false) .$ true

isLowerCase :: Skully (Char -> Bool b)
isLowerCase = s .$ (c .$ and .$ isGreaterThanBacktick) .$ isLessThanLeftBrace

toUpperCase :: Skully (Char -> Char)
toUpperCase = d .$ (s .$ (c .$ ifB .$ isLowerCase) .$ capitalize)

step :: Skully (Unit b -> Unit b)
step = c .$ (l .$ unit) .$ (c .$ (f .$ c .$ toUpperCase) .$ (f .$ u))

program :: Skully (Unit b)
program = y .$ step

main :: IO ()
main = do
    let original = program
    putStrLn "--- original"
    print original

    let optimized = optimize original
    putStrLn "--- optimized"
    print optimized

    putStrLn "--- execution (input lines as necessary)"
    void (eval optimized)
