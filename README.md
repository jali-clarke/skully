# skully

A super simple, Turing-complete language + interpreter (+ compiler eventually?  maybe) based around polymorphic typed [SK combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).  Not at all designed to be written by hand.  Looks awfully similar to [Unlambda](https://en.wikipedia.org/wiki/Unlambda).

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
```

## Grammar

```
<expr> ::= s | k | u | l | y | <expr><expr> | (<expr>) | <char>
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
u c a = putchar(c), a
```

Similar to `k`, but outputs the char `c` to `stdout`.  Second argument is simply returned.

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
