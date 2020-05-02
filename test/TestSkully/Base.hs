module TestSkully.Base (
    testSkullyBase
) where

import Prelude hiding (getChar, putChar)

import Test.Hspec
import Helpers

import Skully.Base

testShowSkully :: Spec
testShowSkully = 
    describe "show :: Skully a -> String" $ do
        it "prints s as \"s\"" $ show s `shouldBe` "s"
        it "prints k as \"k\"" $ show k `shouldBe` "k"
        it "prints u as \"u\"" $ show u `shouldBe` "u"
        it "prints l as \"l\"" $ show l `shouldBe` "l"
        it "prints y as \"y\"" $ show y `shouldBe` "y"
        it "prints q as \"q\"" $ show q `shouldBe` "q"
        it "prints e as \"e\"" $ show e `shouldBe` "e"
        it "prints s .$ s as \"ss\"" $ show (s .$ s) `shouldBe` "ss"
        it "prints s .$ q as \"sq\"" $ show (s .$ q) `shouldBe` "sq"
        it "prints k .$ q as \"kq\"" $ show (k .$ q) `shouldBe` "kq"
        it "prints s .$ (k .$ k) as \"s(kk)\"" $ show (s .$ (k .$ k)) `shouldBe` "s(kk)"
        it "prints s .$ k .$ k as \"skk\"" $ show (s .$ k .$ k) `shouldBe` "skk"
        it "prints char 'x' as \"'x'\"" $ show (char 'x') `shouldBe` "'x'"
        it "prints char 'y' as \"'y'\"" $ show (char 'y') `shouldBe` "'y'"
        it "displays char '\\x11' as '\\x11' in exprs" $ show (char '\x11') `shouldBe` "'\\x11'"
        it "displays char '\\xff' as '\\xff' in exprs" $ show (char '\xff') `shouldBe` "'\\xff'"
        it "displays char '\\x20' as ' ' in exprs" $ show (char '\x20') `shouldBe` "' '"
        it "displays char '\\x7e' as '~' in exprs" $ show (char '\x7e') `shouldBe` "'~'"
        it "displays char '\\x1f' as '\\x1f' in exprs" $ show (char '\x1f') `shouldBe` "'\\x1f'"
        it "displays char '\\x7f' as '\\x7f' in exprs" $ show (char '\x7f') `shouldBe` "'\\x7f'"

testEqSkully :: Spec
testEqSkully =
    describe "(==) :: Skully a -> Skully a -> Bool" $ do
        it "should consider char 'x' and char 'x' to be equal" $ char 'x' `shouldBe` char 'x'
        it "should consider char 'x' and char 'y' to be unequal" $ char 'x' `shouldNotBe` char 'y'
        it "should consider char 'z' and char 'z' to be equal" $ char 'z' `shouldBe` char 'z'
        it "should consider char 'x' and char 'z' to be unequal" $ char 'x' `shouldNotBe` char 'z'
        it "should consider s and s to be equal" $ s `shouldBe` s
        it "should consider k and k to be equal" $ k `shouldBe` k
        it "should consider u and u to be equal" $ u `shouldBe` u
        it "should consider l and l to be equal" $ l `shouldBe` l
        it "should consider y and y to be equal" $ y `shouldBe` y
        it "should consider q and q to be equal" $ q `shouldBe` q
        it "should consider e and e to be equal" $ e `shouldBe` e
        it "should consider s .$ k and s .$ k to be equal" $ (s .$ k) `shouldBe` (s .$ k)
        it "should consider s .$ q and s .$ k to be unequal" $ (s .$ q) `shouldNotBe` (s .$ k)
        it "should consider s .$ k and s .$ u to be unequal" $ (s .$ k) `shouldNotBe` (s .$ u)
        it "should consider k .$ k and k .$ u to be unequal" $ (k .$ k) `shouldNotBe` (k .$ u)

testEvalSkullyChar :: Spec
testEvalSkullyChar =
    describe "eval-ing char expressions" $ do
        it "is a no-op when evaluating char 'x'" $
            withStreamsShouldReturn ("", "") (char 'x', ("", "")) $
                char 'x'
        it "is a no-op when evaluating char '\x7e'" $
            withStreamsShouldReturn ("", "") (char '\x7e', ("", "")) $
                char '\x7e'

testOptimizeSkullyChar :: Spec
testOptimizeSkullyChar =
    describe "optimizing char expressions" $ do
        it "is a no-op when optimizing char 'x'" $ optimize (char 'x') `shouldBe` char 'x'
        it "is a no-op when optimizing char '\x7e'" $ optimize (char '\x7e') `shouldBe` char '\x7e'

testEvalSkullyS :: Spec
testEvalSkullyS =
    describe "eval-ing s expressions" $ do
        it "is a no-op when evaluating s" $
            withStreamsShouldReturn ("", "") (s, ("", "")) $
                s
        it "uses its argument appropriately when evaluating s .$ k .$ k .$ y" $ do
            withStreamsShouldReturn ("", "") (y, ("", "")) $
                s .$ k .$ k .$ y
        it "evaluates its result when evaluating s .$ u .$ k .$ char 'x'" $ do
            withStreamsShouldReturn ("", "") (k .$ char 'x', ("", "x")) $
                s .$ u .$ k .$ char 'x'
        it "is a no-op when evaluated with only two arguments in s .$ k .$ k" $ do
            withStreamsShouldReturn ("", "") (s .$ k .$ k, ("", "")) $
                s .$ k .$ k
        it "is a no-op when evaluated with only one argument in s .$ k" $ do
            withStreamsShouldReturn ("", "") (s .$ k, ("", "")) $
                s .$ k

testOptimizeSkullyS :: Spec
testOptimizeSkullyS =
    describe "optimizing s expressions" $ do
        it "is a no-op when optimizing s" $ optimize s `shouldBe` s
        it "evaluates its result when optimizing s .$ k .$ k .$ u" $ optimize (s .$ k .$ k .$ u) `shouldBe` u
        it "is a no-op when optimized with only two arguments in s .$ k .$ k" $ optimize (s .$ k .$ k) `shouldBe` s .$ k .$ k
        it "is a no-op when optimized with only one argument in s .$ k" $ optimize (s .$ k) `shouldBe` s .$ k
        it "deep-optimizes both args when optimized with only two arguments in s .$ (s .$ k .$ k .$ k) .$ (k .$ q .$ u)" $
            optimize (s .$ (s .$ k .$ k .$ k) .$ (k .$ q .$ u)) `shouldBe` s .$ k .$ q
        it "deep-optimizes arg when optimized with only one argument in s .$ (k .$ s .$ char 'x')" $
            optimize (s .$ (k .$ s .$ char 'x')) `shouldBe` s .$ s

testEvalSkullyK :: Spec
testEvalSkullyK =
    describe "eval-ing k expressions" $ do
        it "returns the first argument when evaluating k .$ s .$ k" $
            withStreamsShouldReturn ("", "") (s, ("", "")) $
                k .$ s .$ k
        it "returns the first argument when evaluating k .$ u .$ (u .$ char 'y' .$ q)" $
            withStreamsShouldReturn ("", "") (u, ("", "")) $
                k .$ u .$ (u .$ char 'y' .$ q)
        it "evaluates the first argument when evaluating k .$ (u .$ char 'x' .$ k) .$ s" $
            withStreamsShouldReturn ("", "") (k, ("", "x")) $
                k .$ (u .$ char 'x' .$ k) .$ s
        it "is a no-op when evaluating with only one argument in k .$ (u .$ char 'x' .$ k)" $
            withStreamsShouldReturn ("", "") (k .$ (u .$ char 'x' .$ k), ("", "")) $
                k .$ (u .$ char 'x' .$ k)

testOptimizeSkullyK :: Spec
testOptimizeSkullyK =
    describe "optimizing k expressions" $ do
        it "returns the first argument when optimizing k .$ s .$ k" $ optimize (k .$ s .$ k) `shouldBe` s
        it "returns the first argument when optimizing k .$ u .$ (u .$ char 'y' .$ q)" $
            optimize (k .$ u .$ (u .$ char 'y' .$ q)) `shouldBe` u
        it "optimizes its result in k .$ (k .$ char 'x' .$ k) .$ s" $
            optimize (k .$ (k .$ char 'x' .$ k) .$ s) `shouldBe` char 'x'
        it "is a no-op when optimizing with only one argument in k .$ u" $ optimize (k .$ u) `shouldBe` k .$ u
        it "is deep-evaluates its argument when optimizing with only one argument in k .$ (k .$ s .$ y)" $
            optimize (k .$ (k .$ s .$ y)) `shouldBe` k .$ s

testEvalSkullyU :: Spec
testEvalSkullyU =
    describe "eval-ing u expressions" $ do
        it "outputs 'x' when evaluating u .$ char 'x' .$ k" $
            withStreamsShouldReturn ("", "") (k, ("", "x")) $
                u .$ char 'x' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ k" $
            withStreamsShouldReturn ("", "") (k, ("", "y")) $
                u .$ char 'y' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ s" $
            withStreamsShouldReturn ("", "") (s, ("", "y")) $
                u .$ char 'y' .$ s
        it "outputs 'x' and then 'y' when evaluating u .$ char 'x' .$ (u .$ char 'y' .$ l)" $
            withStreamsShouldReturn ("", "") (l, ("", "xy")) $
                u .$ char 'x' .$ (u .$ char 'y' .$ l)
        it "evaluates its char arg to produce a char when evaluating u .$ (k .$ char 'x' .$ k) .$ s" $
            withStreamsShouldReturn ("", "") (s, ("", "x")) $
                u .$ (k .$ char 'x' .$ k) .$ s
        it "is a no-op when evaluating with only one argument in u .$ (u .$ char 'x' .$ char 'z')" $
            withStreamsShouldReturn ("", "") (u .$ (u .$ char 'x' .$ char 'z'), ("", "")) $
                u .$ (u .$ char 'x' .$ char 'z')

testOptimizeSkullyU :: Spec
testOptimizeSkullyU =
    describe "optimizing u expressions" $ do
        it "deep-evaluates both arguments when applied to two arguments in u .$ (k .$ char 'x' .$ y) .$ (s .$ k .$ k .$ q)" $
            optimize (u .$ (k .$ char 'x' .$ y) .$ (s .$ k .$ k .$ q)) `shouldBe` u .$ char 'x' .$ q
        it "deep-evaluates its argument when applied to one argument in u .$ (s .$ k .$ u .$ char 'x')" $
            optimize (u .$ (s .$ k .$ u .$ char 'x')) `shouldBe` u .$ char 'x'

testEvalSkullyL :: Spec
testEvalSkullyL =
    describe "eval-ing l expressions" $ do
        it "captures the first char from stdin and injects it when evaluating l .$ k" $
            withStreamsShouldReturn ("x", "") (k .$ char 'x', ("", "")) $
                l .$ k
        it "captures the first (different) char from stdin and injects it when evaluating l .$ u" $
            withStreamsShouldReturn ("y", "") (u .$ char 'y', ("", "")) $
                l .$ u
        it "evaluates the resulting expression after capturing the char when evaluating l .$ (k .$ s)" $
            withStreamsShouldReturn ("a", "") (s, ("", "")) $
                l .$ (k .$ s)

testOptimizeSkullyL :: Spec
testOptimizeSkullyL =
    describe "optimizes l expressions" $ do
        it "deep-optimzes its argument in l .$ (k .$ k .$ q)" $ optimize (l .$ (k .$ k .$ q)) `shouldBe` l .$ k

testEvalSkullyY :: Spec
testEvalSkullyY =
    describe "eval-ing y expressions" $ do
        it "passes itself into its argument when evaluating y .$ (s .$ k)" $
            withStreamsShouldReturn ("", "") (s .$ k .$ (y .$ (s .$ k)), ("", "")) $
                y .$ (s .$ k)
        it "passes itself into its argument and then evalutes the result when evaluating y .$ (k .$ char 'c')" $
            withStreamsShouldReturn ("", "") (char 'c', ("", "")) $
                y .$ (k .$ char 'c')

testOptimizeSkullyY :: Spec
testOptimizeSkullyY =
    describe "optimizing y expressions" $ do
        it "deep-optimizes its argument in y .$ (k .$ (s .$ k) .$ k)" $
            optimize (y .$ (k .$ (s .$ k) .$ k)) `shouldBe` y .$ (s .$ k)
        it "optimizes away y when arg is a partially-applied k expr in y .$ (k .$ (k .$ char 'x' .$ y))" $
            optimize (y .$ (k .$ (k .$ char 'x') .$ y)) `shouldBe` char 'x'
        it "optimizes away y when arg is equivalent to a partially-applied k expr in y .$ (k .$ (k .$ s) .$ u)" $
            optimize (y .$ (k .$ (k .$ s) .$ u)) `shouldBe` s

testEvalSkullyQ :: Spec
testEvalSkullyQ =
    describe "eval-ing q expressions" $ do
        it "decs and incs its char argument and passes both to its second argument when char arg is simple char in q .$ char 'c' .$ e" $
            withStreamsShouldReturn ("", "") (e .$ char 'b' .$ char 'd', ("", "")) $
                q .$ char 'c' .$ e
        it "evals its char arg when it is not a simple char in q .$ (l .$ (s .$ k .$ k)) .$ e" $
            withStreamsShouldReturn ("j", "") (e .$ char 'i' .$ char 'k', ("", "")) $
                q .$ (l .$ (s .$ k .$ k)) .$ e
        it "evals its result in q .$ char 'y' .$ u" $
            withStreamsShouldReturn ("", "") (char 'z', ("", "x")) $
                q .$ char 'y' .$ u
        it "underflows its char arg if it is \\x00 in q .$ char '\\x00' .$ e" $
            withStreamsShouldReturn ("", "") (e .$ char '\xff' .$ char '\x01', ("", "")) $
                q .$ char '\x00' .$ e
        it "overflows its char arg if it is \\xff in q .$ char '\\xff' .$ e" $
            withStreamsShouldReturn ("", "") (e .$ char '\xfe' .$ char '\x00', ("", "")) $
                q .$ char '\xff' .$ e
        it "is a no-op when evaluating with only one arg in q .$ (l .$ (s .$ k .$ k))" $
            withStreamsShouldReturn ("", "") (q .$ (l .$ (s .$ k .$ k)), ("", "")) $
                q .$ (l .$ (s .$ k .$ k))

testOptimizeSkullyQ :: Spec
testOptimizeSkullyQ =
    describe "optimizing q expressions" $ do
        it "decs and incs its char argument and passes both to its second argument when char arg is simple char in q .$ char 'c' .$ e" $
            optimize (q .$ char 'c' .$ e) `shouldBe` e .$ char 'b' .$ char 'd'
        it "optimizes its char arg when it is not a simple char in q .$ (k .$ char 'j' .$ u) .$ e" $
            optimize (q .$ (k .$ char 'j' .$ u) .$ e) `shouldBe` e .$ char 'i' .$ char 'k'
        it "optimizes its result in q .$ char 'y' .$ k" $
            optimize (q .$ char 'y' .$ k) `shouldBe` char 'x'
        it "underflows its char arg if it is \\x00 in q .$ char '\\x00' .$ e" $
            optimize (q .$ char '\x00' .$ e) `shouldBe` e .$ char '\xff' .$ char '\x01'
        it "overflows its char arg if it is \\xff in q .$ char '\\xff' .$ e" $
            optimize (q .$ char '\xff' .$ e) `shouldBe` e .$ char '\xfe' .$ char '\x00'
        it "deep-evaluates its only arg when optimizing with only one arg in q .$ (s .$ k .$ k .$ char 'x')" $
            optimize (q .$ (s .$ k .$ k .$ char 'x')) `shouldBe` q .$ char 'x'
        it "deep-evaluates both args when optimizing with unoptimizable arg in q .$ (k .$ (l .$ (s .$ k .$ k)) .$ u) .$ (s .$ k .$ k .$ e)" $
            optimize (q .$ (k .$ (l .$ (s .$ k .$ k)) .$ u) .$ (s .$ k .$ k .$ e)) `shouldBe` q .$ (l .$ (s .$ k .$ k)) .$ e

testEvalSkullyE :: Spec
testEvalSkullyE =
    describe "eval-ing e expressions" $ do
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z'" $
            withStreamsShouldReturn ("", "") (char 'x', ("", "")) $
                e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z'
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'" $
            withStreamsShouldReturn ("", "") (char 'z', ("", "")) $
                e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'
        it "returns and evals the second non-char argument when the first argument is equal to the second in expr e .$ char 'j' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'" $
            withStreamsShouldReturn ("", "") (char 'y', ("", "q")) $
                e .$ char 'j' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'
        it "returns and evals the third non-char argument when the first argument is greater than the second in expr e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')" $
            withStreamsShouldReturn ("", "") (char 'c', ("", "")) $
                e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')
        it "evaluates its first char arg if it is not a simple char in expr e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y" $
            withStreamsShouldReturn ("", "") (k .$ char 'x', ("", "")) $
                e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y
        it "evaluates its second char arg if it is not a simple char in expr e .$ char 'x' .$ (l .$ (s .$ k .$ k)) .$ y .$ .y .$ (k .$ char 'x')" $
            withStreamsShouldReturn ("t", "") (k .$ char 'x', ("", "")) $
                e .$ char 'x' .$ (l .$ (s .$ k .$ k)) .$ y .$ y .$ (k .$ char 'x')
        it "is a no-op when evaluated with only 4 args in e .$ char 'x' .$ char 'y' .$ k .$ k" $
            withStreamsShouldReturn ("", "") (e .$ char 'x' .$ char 'y' .$ k .$ k, ("", "")) $
                e .$ char 'x' .$ char 'y' .$ k .$ k
        it "is a no-op when evaluated with only 3 args in e .$ char 'x' .$ (u .$ char 'z' .$ char 'y') .$ k" $
            withStreamsShouldReturn ("", "") (e .$ char 'x' .$ (u .$ char 'z' .$ char 'y') .$ k, ("", "")) $
                e .$ char 'x' .$ (u .$ char 'z' .$ char 'y') .$ k
        it "is a no-op when evaluated with only 2 args in e .$ char 'x' .$ (u .$ char 'z' .$ char 'y')" $
            withStreamsShouldReturn ("", "") (e .$ char 'x' .$ (u .$ char 'z' .$ char 'y'), ("", "")) $
                e .$ char 'x' .$ (u .$ char 'z' .$ char 'y')
        it "is a no-op when evaluated with only 1 arg in e .$ char 'x'" $
            withStreamsShouldReturn ("", "") (e .$ char 'x', ("", "")) $
                e .$ char 'x'

testOptimizeSkullyE :: Spec
testOptimizeSkullyE =
    describe "optimizing e expressions" $ do
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z'" $
            optimize (e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z') `shouldBe` char 'x'
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'" $
            optimize (e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p') `shouldBe` char 'z'
        it "returns and optimizes the second non-char argument when the first argument is equal to the second in expr e .$ char 'j' .$ char 'j' .$ char 'z' .$ (k .$ char 'q' .$ char 'y') .$ char 'p'" $
            optimize (e .$ char 'j' .$ char 'j' .$ char 'z' .$ (k .$ char 'q' .$ char 'y') .$ char 'p') `shouldBe` char 'q'
        it "returns and optimizes the third non-char argument when the first argument is greater than the second in expr e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')" $
            optimize (e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')) `shouldBe` char 'c'
        it "optimizes its first char arg if it is not a simple char in expr e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y" $
            optimize (e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y) `shouldBe` k .$ char 'x'
        it "optimizes its second char arg if it is not a simple char in expr e .$ char 'x' .$ (s .$ k .$ k .$ char 't') .$ y .$ .y .$ (k .$ char 'x')" $
            optimize (e .$ char 'x' .$ (s .$ k .$ k .$ char 't') .$ y .$ y .$ (k .$ char 'x')) `shouldBe` k .$ char 'x'
        it "deep-optimizes all args when optimized with only 4 args in e .$ (i .$ char 'x') .$ (i .$ char 'y') .$ (i .$ k) .$ (i .$ k)" $
            let i = s .$ k .$ k
            in optimize (e .$ (i .$ char 'x') .$ (i .$ char 'y') .$ (i .$ k) .$ (i .$ k)) `shouldBe` e .$ char 'x' .$ char 'y' .$ k .$ k
        it "deep-optimizes when optimized with only 3 args in e .$ (i .$ char 'x') .$ (k .$ char 'z' .$ char 'y') .$ (i .$ k)" $
            let i = s .$ k .$ k
            in optimize (e .$ (i .$ char 'x') .$ (k .$ char 'z' .$ char 'y') .$ (i .$ k)) `shouldBe` e .$ char 'x' .$ char 'z' .$ k
        it "deep-optimizes when optimized with only 2 args in e .$ (i .$ char 'x') .$ (k .$ char 'z' .$ char 'y')" $
            let i = s .$ k .$ k
            in optimize (e .$ (i .$ char 'x') .$ (k .$ char 'z' .$ char 'y')) `shouldBe` e .$ char 'x' .$ char 'z'
        it "deep-optimizes when optimized with only 1 arg in e .$ (i .$ char 'x')" $
            let i = s .$ k .$ k
            in optimize (e .$ (i .$ char 'x')) `shouldBe` e .$ char 'x'
        it "deep-optimizes all args when optimized with non-optimizable chars in e .$ (i .$ l .$ i) .$ (i .$ l .$ i) .$ (i .$ k) .$ (i .$ k) .$ (i .$ k)" $
            let i = s .$ k .$ k
            in optimize (e .$ (i .$ l .$ i) .$ (i .$ l .$ i) .$ (i .$ k) .$ (i .$ k) .$ (i .$ k)) `shouldBe` e .$ (l .$ i) .$ (l .$ i) .$ k .$ k .$ k

testEvalSkullyAp :: Spec
testEvalSkullyAp =
    describe "eval-ing nested Ap expressions not yet covered" $ do
        it "evaluates nested expression in k .$ u .$ char 'x' .$ char 'y' .$ s" $
            withStreamsShouldReturn ("", "") (s, ("", "y")) $
                k .$ u .$ char 'x' .$ char 'y' .$ s
        it "does hello world example 1" $
            withStreamsShouldReturn ("", "") (k, ("", "Hello world!")) $
                let skullyPutChar c = u .$ char c
                in foldr (.$) k (fmap skullyPutChar "Hello world!")
        it "does hello world example 2" $
            withStreamsShouldReturn ("", "") (k, ("", "Hello world!")) $
                let skullyPutChar c = u .$ char c
                in skullyPutChar 'H' .$ skullyPutChar 'e' .$ skullyPutChar 'l' .$ skullyPutChar 'l' .$ skullyPutChar 'o' .$ skullyPutChar ' ' .$
                    skullyPutChar 'w' .$ skullyPutChar 'o' .$ skullyPutChar 'r' .$ skullyPutChar 'l' .$ skullyPutChar 'd' .$ skullyPutChar '!' .$ k

testOptimizeSkullyAp :: Spec
testOptimizeSkullyAp =
    describe "optimizing nested Ap expressions not yet covered" $ do
        it "optimizes nested expression in k .$ k .$ char 'x' .$ char 'y' .$ s" $
            optimize (k .$ k .$ char 'x' .$ char 'y' .$ s) `shouldBe` char 'y'

testEvalSkully :: Spec
testEvalSkully =
    describe "eval :: CharSocket m => Skully a -> m (Skully a)" $ do
        testEvalSkullyChar
        testEvalSkullyS
        testEvalSkullyK
        testEvalSkullyU
        testEvalSkullyL
        testEvalSkullyY
        testEvalSkullyQ
        testEvalSkullyE
        testEvalSkullyAp

testOptimizeSkully :: Spec
testOptimizeSkully =
    describe "optimize :: Skully a -> Skully a" $ do
        testOptimizeSkullyChar
        testOptimizeSkullyS
        testOptimizeSkullyK
        testOptimizeSkullyU
        testOptimizeSkullyL
        testOptimizeSkullyY
        testOptimizeSkullyQ
        testOptimizeSkullyE
        testOptimizeSkullyAp

testSkullyBase :: Spec
testSkullyBase =
    describe "Skully.Base" $ do
        testShowSkully
        testEqSkully
        testEvalSkully
        testOptimizeSkully
