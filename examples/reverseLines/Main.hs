module Main where

import Prelude hiding (reverse)

import Data.Functor (void)
import Skully.Stdlib

string :: String -> Skully (List b Char)
string str =
    case str of
        [] -> nil
        x : rest -> cons .$ char x .$ string rest

puts :: Skully (List a Char -> a -> a)
puts = f .$ (c .$ f .$ withList) .$ u

isNewline :: Skully (Char -> a -> a -> a)
isNewline = c .$ (c .$ d) .$ (c .$ f .$ (e .$ char '\n'))

withLine :: Skully ((List b Char -> a) -> a)
withLine = y .$ (c .$ (c .$ l) .$ (c .$ (s .$ (c .$ s .$ (c .$ (f .$ isNewline) .$ (a .$ nil)))) .$ (c .$ (f .$ c .$ (c .$ (f .$ c .$ cons) .$ c)) .$ c)))

lineBreak :: Skully (a -> a)
lineBreak = u .$ char '\n'

getLineAndReverse :: Skully (a -> a)
getLineAndReverse = c .$ (puts .$ string "Input line: ") .$ (c .$ withLine .$ (c .$ (c .$ (puts .$ string "Reversed  : ")) .$ (f .$ (c .$ c .$ (c .$ (f .$ puts) .$ (c .$ lineBreak .$ lineBreak))) .$ reverse)))

program :: Skully a
program = y .$ getLineAndReverse

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
