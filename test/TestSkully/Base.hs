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

testEvalSkullyChar :: Spec
testEvalSkullyChar =
    describe "eval-ing char expressions" $ do
        it "is a no-op when evaluating char 'x'" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                char 'x'
        it "is a no-op when evaluating char '\x7e'" $
            withStreamsShouldReturn ("", "") ("'~'", ("", "")) $
                char '\x7e'

testOptimizeSkullyChar :: Spec
testOptimizeSkullyChar =
    describe "optimizing char expressions" $ do
        it "is a no-op when optimizing char 'x'" $ show (optimize $ char 'x') `shouldBe` "'x'"
        it "is a no-op when optimizing char '\x7e'" $ show (optimize $ char '\x7e') `shouldBe` "'~'"

testEvalSkullyS :: Spec
testEvalSkullyS =
    describe "eval-ing s expressions" $ do
        it "is a no-op when evaluating s" $
            withStreamsShouldReturn ("", "") ("s", ("", "")) $
                s
        it "uses its argument appropriately when evaluating s .$ k .$ k .$ y" $ do
            withStreamsShouldReturn ("", "") ("y", ("", "")) $
                s .$ k .$ k .$ y
        it "evaluates its result when evaluating s .$ u .$ k .$ char 'x'" $ do
            withStreamsShouldReturn ("", "") ("k'x'", ("", "x")) $
                s .$ u .$ k .$ char 'x'
        it "is a no-op when evaluated with only two arguments in s .$ k .$ k" $ do
            withStreamsShouldReturn ("", "") ("skk", ("", "")) $
                s .$ k .$ k
        it "is a no-op when evaluated with only one argument in s .$ k" $ do
            withStreamsShouldReturn ("", "") ("sk", ("", "")) $
                s .$ k

testOptimizeSkullyS :: Spec
testOptimizeSkullyS =
    describe "optimizing s expressions" $ do
        it "is a no-op when optimizing s" $ show (optimize s) `shouldBe` "s"
        it "evaluates its result when optimizing s .$ k .$ k .$ u" $ show (optimize $ s .$ k .$ k .$ u) `shouldBe` "u"
        it "is a no-op when optimized with only two arguments in s .$ k .$ k" $ show (optimize $ s .$ k .$ k) `shouldBe` "skk"
        it "is a no-op when optimized with only one argument in s .$ k" $ show (optimize $ s .$ k) `shouldBe` "sk"

testEvalSkullyK :: Spec
testEvalSkullyK =
    describe "eval-ing k expressions" $ do
        it "returns the first argument when evaluating k .$ s .$ k" $
            withStreamsShouldReturn ("", "") ("s", ("", "")) $
                k .$ s .$ k
        it "returns the first argument when evaluating k .$ u .$ (u .$ char 'y' .$ q)" $
            withStreamsShouldReturn ("", "") ("u", ("", "")) $
                k .$ u .$ (u .$ char 'y' .$ q)
        it "evaluates the first argument when evaluating k .$ (u .$ char 'x' .$ k) .$ s" $
            withStreamsShouldReturn ("", "") ("k", ("", "x")) $
                k .$ (u .$ char 'x' .$ k) .$ s
        it "is a no-op when evaluating with only one argument in k .$ (u .$ char 'x' .$ k)" $
            withStreamsShouldReturn ("", "") ("k(u'x'k)", ("", "")) $
                k .$ (u .$ char 'x' .$ k)

testOptimizeSkullyK :: Spec
testOptimizeSkullyK =
    describe "optimizing k expressions" $ do
        it "returns the first argument when optimizing k .$ s .$ k" $ show (optimize $ k .$ s .$ k) `shouldBe` "s"
        it "returns the first argument when optimizing k .$ u .$ (u .$ char 'y' .$ q)" $
            show (optimize $ k .$ u .$ (u .$ char 'y' .$ q)) `shouldBe` "u"
        it "optimizes its result in k .$ (k .$ char 'x' .$ k) .$ s" $
            show (optimize $ k .$ (k .$ char 'x' .$ k) .$ s) `shouldBe` "'x'"

testEvalSkullyU :: Spec
testEvalSkullyU =
    describe "eval-ing u expressions" $ do
        it "outputs 'x' when evaluating u .$ char 'x' .$ k" $
            withStreamsShouldReturn ("", "") ("k", ("", "x")) $
                u .$ char 'x' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ k" $
            withStreamsShouldReturn ("", "") ("k", ("", "y")) $
                u .$ char 'y' .$ k
        it "outputs 'y' when evaluating u .$ char 'y' .$ s" $
            withStreamsShouldReturn ("", "") ("s", ("", "y")) $
                u .$ char 'y' .$ s
        it "outputs 'x' and then 'y' when evaluating u .$ char 'x' .$ (u .$ char 'y' .$ l)" $
            withStreamsShouldReturn ("", "") ("l", ("", "xy")) $
                u .$ char 'x' .$ (u .$ char 'y' .$ l)
        it "evaluates its char arg to produce a char when evaluating u .$ (k .$ char 'x' .$ k) .$ s" $
            withStreamsShouldReturn ("", "") ("s", ("", "x")) $
                u .$ (k .$ char 'x' .$ k) .$ s
        it "is a no-op when evaluating with only one argument in u .$ (u .$ char 'x' .$ char 'z')" $
            withStreamsShouldReturn ("", "") ("u(u'x''z')", ("", "")) $
                u .$ (u .$ char 'x' .$ char 'z')

testEvalSkullyL :: Spec
testEvalSkullyL =
    describe "eval-ing l expressions" $ do
        it "captures the first char from stdin and injects it when evaluating l .$ k" $
            withStreamsShouldReturn ("x", "") ("k'x'", ("", "")) $
                l .$ k
        it "captures the first (different) char from stdin and injects it when evaluating l .$ u" $
            withStreamsShouldReturn ("y", "") ("u'y'", ("", "")) $
                l .$ u
        it "evaluates the resulting expression after capturing the char when evaluating l .$ (k .$ s)" $
            withStreamsShouldReturn ("a", "") ("s", ("", "")) $
                l .$ (k .$ s)

testEvalSkullyY :: Spec
testEvalSkullyY =
    describe "eval-ing y expressions" $ do
        it "passes itself into its argument when evaluating y .$ (s .$ k)" $
            withStreamsShouldReturn ("", "") ("sk(y(sk))", ("", "")) $
                y .$ (s .$ k)
        it "passes itself into its argument and then evalutes the result when evaluating y .$ (k .$ char 'c')" $
            withStreamsShouldReturn ("", "") ("'c'", ("", "")) $
                y .$ (k .$ char 'c')

testEvalSkullyQ :: Spec
testEvalSkullyQ =
    describe "eval-ing q expressions" $ do
        it "decs and incs its char argument and passes both to its second argument when char arg is simple char in q .$ char 'c' .$ e" $
            withStreamsShouldReturn ("", "") ("e'b''d'", ("", "")) $
                q .$ char 'c' .$ e
        it "evals its char arg when it is not a simple char in q .$ (l .$ (s .$ k .$ k)) .$ e" $
            withStreamsShouldReturn ("j", "") ("e'i''k'", ("", "")) $
                q .$ (l .$ (s .$ k .$ k)) .$ e
        it "evals its result in q .$ char 'y' .$ u" $
            withStreamsShouldReturn ("", "") ("'z'", ("", "x")) $
                q .$ char 'y' .$ u
        it "underflows its char arg if it is \\x00 in q .$ char '\\x00' .$ e" $
            withStreamsShouldReturn ("", "") ("e'\\xff''\\x01'", ("", "")) $
                q .$ char '\x00' .$ e
        it "overflows its char arg if it is \\xff in q .$ char '\\xff' .$ e" $
            withStreamsShouldReturn ("", "") ("e'\\xfe''\\x00'", ("", "")) $
                q .$ char '\xff' .$ e
        it "is a no-op when evaluating with only one arg in q .$ (l .$ (s .$ k .$ k))" $
            withStreamsShouldReturn ("", "") ("q(l(skk))", ("", "")) $
                q .$ (l .$ (s .$ k .$ k))

testEvalSkullyE :: Spec
testEvalSkullyE =
    describe "eval-ing e expressions" $ do
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z'" $
            withStreamsShouldReturn ("", "") ("'x'", ("", "")) $
                e .$ char 'a' .$ char 'b' .$ char 'x' .$ char 'y' .$ char 'z'
        it "returns the first non-char argument when the first argument is less than the second in expr e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'" $
            withStreamsShouldReturn ("", "") ("'z'", ("", "")) $
                e .$ char 't' .$ char 'u' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'
        it "returns and evals the second non-char argument when the first argument is equal to the second in expr e .$ char 'j' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'" $
            withStreamsShouldReturn ("", "") ("'y'", ("", "q")) $
                e .$ char 'j' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ char 'p'
        it "returns and evals the third non-char argument when the first argument is greater than the second in expr e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')" $
            withStreamsShouldReturn ("", "") ("'c'", ("", "")) $
                e .$ char 'k' .$ char 'j' .$ char 'z' .$ (u .$ char 'q' .$ char 'y') .$ (s .$ k .$ k .$ char 'c')
        it "evaluates its first char arg if it is not a simple char in expr e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y" $
            withStreamsShouldReturn ("", "") ("k'x'", ("", "")) $
                e .$ (s .$ k .$ k .$ char 'x') .$ char 'y' .$ (k .$ char 'x') .$ y .$ y
        it "evaluates its second char arg if it is not a simple char in expr e .$ char 'x' .$ (l .$ (s .$ k .$ k)) .$ y .$ .y .$ (k .$ char 'x')" $
            withStreamsShouldReturn ("t", "") ("k'x'", ("", "")) $
                e .$ char 'x' .$ (l .$ (s .$ k .$ k)) .$ y .$ y .$ (k .$ char 'x')
        it "is a no-op when evaluated with only 4 args in e .$ char 'x' .$ char 'y' .$ k .$ k" $
            withStreamsShouldReturn ("", "") ("e'x''y'kk", ("", "")) $
                e .$ char 'x' .$ char 'y' .$ k .$ k
        it "is a no-op when evaluated with only 3 args in e .$ char 'x' .$ (u .$ char 'z' .$ char 'y') .$ k" $
            withStreamsShouldReturn ("", "") ("e'x'(u'z''y')k", ("", "")) $
                e .$ char 'x' .$ (u .$ char 'z' .$ char 'y') .$ k
        it "is a no-op when evaluated with only 2 args in e .$ char 'x' .$ (u .$ char 'z' .$ char 'y')" $
            withStreamsShouldReturn ("", "") ("e'x'(u'z''y')", ("", "")) $
                e .$ char 'x' .$ (u .$ char 'z' .$ char 'y')
        it "is a no-op when evaluated with only 1 arg in e .$ char 'x'" $
            withStreamsShouldReturn ("", "") ("e'x'", ("", "")) $
                e .$ char 'x'

testEvalSkullyAp :: Spec
testEvalSkullyAp =
    describe "eval-ing nested Ap expressions not yet covered" $ do
        it "evaluates nested expression in k .$ u .$ char 'x' .$ char 'y' .$ s" $
            withStreamsShouldReturn ("", "") ("s", ("", "y")) $
                k .$ u .$ char 'x' .$ char 'y' .$ s
        it "does hello world example 1" $
            withStreamsShouldReturn ("", "") ("k", ("", "Hello world!")) $
                let skullyPutChar c = u .$ char c
                in foldr (.$) k (fmap skullyPutChar "Hello world!")
        it "does hello world example 2" $
            withStreamsShouldReturn ("", "") ("k", ("", "Hello world!")) $
                let skullyPutChar c = u .$ char c
                in skullyPutChar 'H' .$ skullyPutChar 'e' .$ skullyPutChar 'l' .$ skullyPutChar 'l' .$ skullyPutChar 'o' .$ skullyPutChar ' ' .$
                    skullyPutChar 'w' .$ skullyPutChar 'o' .$ skullyPutChar 'r' .$ skullyPutChar 'l' .$ skullyPutChar 'd' .$ skullyPutChar '!' .$ k

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
        -- testOptimizeSkullyU
        -- testOptimizeSkullyL
        -- testOptimizeSkullyY
        -- testOptimizeSkullyQ
        -- testOptimizeSkullyE
        -- testOptimizeSkullyAp

testSkullyBase :: Spec
testSkullyBase =
    describe "Skully.Base" $ do
        testShowSkully
        testEvalSkully
        testOptimizeSkully
