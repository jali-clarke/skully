# skully

A super simple, Turing-complete language + interpreter (+ compiler eventually?  maybe) based around a lazily evaluated, polymorphically typed [SK combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).  Not at all designed to be written by hand.  Looks awfully similar to [Unlambda](https://en.wikipedia.org/wiki/Unlambda).

Very much a WIP.

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
```

## Grammar

```
<expr> ::= s | k | u | l | y | q | <expr><expr> | (<expr>) | <char>
<char> ::= (any ASCII character)
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

Takes a Char and a callback, and applies the callback to the predecessor and successor of the same Char.  This allows for construction + pattern matching on Char.  If the Char is `\x00`, its predecessor is also `\x00`.  If the Char is `\xff`, its successor is also `\xff`.
