# skully

A super simple, Turing-complete language + interpreter (+ compiler eventually?  maybe) based around a lazily evaluated (sorta), polymorphically typed [SK combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).  Not at all designed to be written by hand.  Looks awfully similar to [Unlambda](https://en.wikipedia.org/wiki/Unlambda).

Very much a WIP.

## Hello world!

Could be worse:

```
u'H'(u'e'(u'l'(u'l'(u'o'(u' '(u'w'(u'o'(u'r'(u'l'(u'd'(u'!'k)))))))))))
```

or, using a more cps approach,

```
(u'H')(u'e')(u'l')(u'l')(u'o')(u' ')(u'w')(u'o')(u'r')(u'l')(u'd')(u'!')k
```

There are a number of ways to implement this.  Have a look at the other sections in this readme to see what exactly is going on up there.

## Types

Not going to formally / rigorously specify anything; this is borrowed from Haskell's syntax.

```
type Char :: *
type (->) :: * -> * -> *

s :: forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
k :: forall a b. a -> b -> a
u :: forall a. Char -> a -> a
l :: forall a. (Char -> a) -> a
y :: forall a. (a -> a) -> a
q :: forall a. Char -> (Char -> Char -> a) -> a
e :: forall a. Char -> Char -> a -> a -> a
```

## Grammar

```
<expr> ::= s | k | u | l | y | q | e | <expr><expr> | (<expr>) | '<char>'
<char> ::= (any ASCII character, including escaped chars and escaped 2-digit hex)
```

Applying one expression to another is done by simply concatenating them.  Application is also left-associative; `abc == (ab)c`.

## Description of Combinators

Definitions are given in pseudocode.

### s

```
s :: forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
s abc ab a = abc a (ab a)
```

Enables composition and argument duplication.

### k

```
k :: forall a b. a -> b -> a
k a b = a
```

Constant function.  Enables ignorance of arguments.

### u

```
u :: forall a. Char -> a -> a
u x a = putchar(x), a
```

Similar to `k`, but outputs the char `x` to `stdout`.  Second argument is simply returned.

### l

```
l :: forall a. (Char -> a) -> a
l g = g (getchar())
```

Takes a callback and applies it to the character fetched from `stdin`.

### y

```
y :: forall a. (a -> a) -> a
y g = g (y g)
```

Takes the fixed point of its argument.  Enables recursion.

### q

```
q :: forall a. Char -> (Char -> Char -> a) -> a
q x g = g (x - 1) (x + 1)
```

Takes a Char and a callback, and applies the callback to the predecessor and successor of the same Char.  This allows for construction + pattern matching on Char.  We allow for overflow and underflow; if the Char is `\x00`, its predecessor is `\xff`.  If the Char is `\xff`, its successor is `\x00`.

### e

```
e :: forall a. Char -> Char -> a -> a -> a -> a
e c0 c1 al ae ag =
    case c0 `compare` c1 of
        LT -> al
        EQ -> ae
        GT -> ag
```

Takes two chars and three expressions.  If the first char is less than the second, return the first expression.  If they are equal, return the second expression.  If the first is greater than the second, return the third expression.

## Stdlib

You can access this by importing `Skully.Stdlib`; it contains some derived combinators for convenience as well as some data types such as `List`s, `Pair`s, and `Either`s.  I really don't expect anyone to use this language to do anything substantial so I'm not going to write much documentation.  Have a look at [Skully.Stdlib](./src/Skully/Stdlib) and its contained files to see what's up - the types should provide enough guidance.  The example program in [app/Main.hs](./app/Main.hs) uses some of these to read lines from stdin and print them reversed.

## Evaluation model

Evaluation is done more or less lazily, in that expressions are reduced from the outside in.  In general, an expression of the form `f x` is evaluated by doing `eval ((eval f) x)`; expression reduction occurs from the outside in.

An example would be: `eval ((K a b) c) == eval (a c)` - note that `b` is completely ignored in this case.  Any side effects arising from evaluating `b` never materialize.  Further, if the reduction of `a c` ends up ignoring `c` entirely, the side effects arising from evaluating `c` never materialize.

Another example is `eval (S a b) == S a b`.  Since `S a b` cannot be further reduced from the outside in, there's nothing else to do.  The side effects arising from reducing `a` and `b` never materialize.

The only exceptions to these rules are the base combinators which take arguments of type `Char`; those arguments are evaluated strictly.