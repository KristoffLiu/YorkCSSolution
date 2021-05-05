```haskell
module Laziness where
import Data.List

```
# Jeremy Jacob 3 Oct 2020
# Laziness

See [Hutton, Chapter 15][1] for a slower exposition, with more
examples.

**Laziness** is an *evaluation strategy* for programs.  It is
sometimes known as "call-by-need".  The strategy evaluates a
sub-expression only at the moment that the value is needed, and only
evaluates it enough to fulful the purpose for which the value is
needed.

## Evaluation strategies

Given an expression there may be many evaluation paths.  Each step of
an evaluation path picks a pattern (which may be a simple name, or a
function application) to **reduce** by using the definition.

It cannot pick any application: consider `(3+1)*2`.  In order to
evaluate a function such as `(*)` _both_ of its arguments must be
fully evaluated.  So the only **redex** (shorthand for **red**ucible
**ex**pression) is `3+1`.  Once this has been evaluated, we have
`4*2`, which again has one redex, that evaluates to `8`.

Consider the trivial definitions:
```haskell

inc, dbl :: Int -> Int
inc x = x + 1
dbl x = x + x

```

and then consider evaluating `dbl(inc 2)`.  There are two redexes, the
applications of `dbl` and of `inc`, that we could choose.  Let's
always evaluate the innermost redex when we have a choice, and
if we have two or more innermost redexes we will disambiguate by
choosing the leftmost:

`dbl(inc 2) ~~> dbl(2 + 1) ~~> dbl 3 ~~> 3 + 3 ~~> 6`

This is called **applicative order** reduction.

Here is an example where there are two innermost redexes:
`(if True then inc else dbl) (2+1)`
The outermost application is not a redex, but both subterms are.  Our
strategy always takes the leftmost, so:

`(if True then inc else dbl) (2+1) ~~> inc (2+1) ~~> inc 3 ~~> 3+1 ~~>4`

Now let's do that again, for the example `dbl(inc 2)`, but always
choose the leftmost-outermost application, or **normal order**
reduction:

`dbl(inc 2) ~~> (inc 2) + (inc 2) ...`

Note that at this point we cannot evaluation the application of `(+)`
because it needs to know the values of its parameters.  Both are at
the same level, so we will take the leftmost before the rightmost:

`...~~> (2+1) + (inc 2) ~~> 3 + (inc 2) ~~> 3 + (2 + 1) ~~> 3 + 3 ~~> 6`

This strategy takes more reductions, but can be improved, by handling
the repetition of `inc 2` better:

`dbl(inc 2) ~~> let x = inc 2 in x + x...`
`...~~> let x = 2+1 in x + x ~~> let x = 3 in x + x ~~> 3 + 3 ~~> 6`

[Exercise: Reduce `(if True then inc else dbl) (2+1)` using normal
order reduction.]

Now consider the strange definition:
```haskell

bottom :: a
bottom = bottom

```

The value `bottom` is the equivalent of
```python
while True:
   pass
```
in Python: it runs forever without doing any useful work.  Its name
comes from the fact that when programs are orderd by utility it is the
least, or bottom, such program.  It is often written `_|_`, as a
rotated letter 'T', which itself is used to stand for `top` or most
useful element.

[Question: is there a `top` program or expression?  If so what is it?
If not, why not?]

Let's add to this the definition of `const`, in a renamed version to
avoid a clash with the name in Prelude:
```haskell

konst :: a -> b -> a
konst    x    y  = x

```
Now let's evaluate `konst 0 bottom` using applicative order reduction.
The leftmost-innermost redex is `bottom`, and reducing that gives us
`bottom` in the leftmost-innermost position.  Hence we get:

`konst 0 bottom ~~> konst 0 bottom ~~> konst 0 bottom ~~> ...`

an evaluation that continues forever.

In contrast, normal order reduction terminates, in one step:

`konst 0 bottom ~~> 0`

There are two important facts (provable as theorems):
1. all terminating reduction paths end in the same value
2. if there are any terminating paths, then normal order reduction is
   a terminating path.

Note that no strategy causes `bottom` to terminate.

[Question: could we design a Turing-equivalent language in which the
definition of values like `bottom` is impossible?]

## Laziness

Laziness is one particular implementation of normal order reduction.
However laziness cannot coexist with _ad hoc_ (side) effects, such as
assignment, and so Haskell does not have assignment.  We will see
later how this is dealt with in pure languages, such as Haskell.
Languages which are not lazy, but implement some kind of applicative
order reduction are called **eager**.

Laziness allows us a new kind of modularity, separating data and
control.  We can generate very large, even potentially infinite, data
structures with only the asymptotic cost of exploring that part of the
data needed for the calculation.  When we generate the data structure
we do not need to know how it will be used, and so we can generate it
in a simpler way.

## Examples

### Simple examples

Here are some simple examples.  The value `nats` is a list of all
natural numbers.  Its definition reflects the Peano axioms for the
natural numbers: a natural number is either 0 or one more than a
natural number.  Another infinite list of numbers is the Fibonacci
sequence: the first two Fibonacci numbers are 1 and 1; the remainder
are found by taking the sum of the previous two Fibonacci numbers.  [I
have provided two variants of each, one using list comprehension, the
other not.  The functions `zip`, `zipWith`, and `tail` are provided
through the `Prelude`; you can check their descriptions using Neil
Mitchell's [Hoogle][2].]
```haskell

nats, nats', fibonacci, fibonacci' :: [Integer]
nats = 0 : [n+1 | n <-nats]
nats' = 0 : map (+1) nats'
fibonacci = 1 : 1 : [a + b | (a,b) <- zip fibonacci (tail fibonacci)]
fibonacci' = 1 : 1 : zipWith (+) fibonacci' (tail fibonacci')

```
Simple control structures can be built by using functions such as
`take`, and `takeWhile` (See the `Prelude` or [Hoogle][2]).  More
complex structures are also available.  Such structures must take care
to only examine a finite part of the infinite data structure.  For
example, the `prelude` operator `(\\) :: Eq a => [a] -> [a] -> [a]`
subtracts every element of its righthand argument from its lefthand
argument once, so `"hello" \\ "l" ~~> "helo"`.  But care is needed if
the lists are infinite.  The expression `[0..]\\[0]` is equivalent to
`[1..]`, because we lazily evaluate the lefthand argument, checking
each time if it is in `[0]` or not.  However `[0]\\[1..]` is
equivalent to `bottom`, because for each element of the lefthand
argument (in this case only `0`), we need to check it against every
element of the righthand argument, and that requires an infinite
number of comparisons.  (Note: both of these examples are equivalent
to `bottom` using an eager evaluation strategy.)

### Eratosthenes's prime sieve

This is a good example of the use of infinite lists that is possible
only because of laziness.

This is a very old algorithm: our earliest known description is from
about 2000 years ago, and attributed to an earlier mathematician,
Eratosthenes of Cryrene, who lived around 2200 years ago.

The algorithm generates all possible primes (until your machine
storage is full!), that is efficient for `small primes'.

We will do this based on `Int`, rather than `Integer`.  While
`Integer` is bounded only by the amount of storage available, `Int` is
more efficient, and bounded by `GHC.Base.maxInt :: Int`; on my machine
`GHC.Base.maxInt = 9,223,372,036,854,775,807`, or over 9 trillion:
that should be sufficient!  Also the `replicate` function requires an
`Int` argument, and does not allow an `Integer` argument; the similar
function `genericReplicate` will work for `Integer` type.  (Question:
why is the cut-down version of `genericReplicate`, `replicate`,
provided, and why is it the preferred version?)

We will give two versions of prime sieves.  Both algorithms take the
current head of the unprocessed list, declare it prime, and then use
the prime to build a sieve that deletes all multiples of the prime.
We can continue this provess as long as the environment requires
another prime.

Our first version, due to David Turner, is, strictly speaking, not
Eratosthenes's algorithm (see [O'Neil][3]).  Its sieves are built
using division.
```haskell

turner :: [Int]
turner = sieve [2..] -- an infinite list
  where sieve (p:ns) = p : sieve [q | q<-ns, q `mod` p /= 0]

```
Our second version is closer to the original.  It marks all products
of a prime for deletion.  Note that we do not need to know the value
of a number marked for deletion, just that it was present.  This makes
it convenient to use `Maybe Int` as internal values in the algorithm;
`Nothing` represents a composite number, and `Just n` a number yet to
be investigated.

We mark by generating an infinite list of functions which are either
`id`, for "do not change the marking", or `const Nothing`, for "mark
as composite".  To mark products of `n` the list of functions repeats
`n-1` occurences of `id` followed by a single occurence of `const
Nothing`.  The function `zipWith f` takes a pair of lists and joins
them element-wise using `f`: `zipWith f [x0, x1, x2,..] [y0, y1,
y2,...]  = [f x0 y0, f x1 y1, f x2 y2,...]`

The joining function, `($)` is "application": `($) f x = f $ x = f x`,
so zipping with `($)` a list of functions and a list of values leaves
us with the result of applying each function to its corrsponding
value.
```haskell

eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing]) 
    sieve (Nothing : ns) = sieve ns

```
We can define many useful other values based on a list of primes.

For example, we can calculate twin prime pairs:
```haskell
twinprime ::[(Int, Int)]
twinprime = [pq | pq@(p, q) <- zip eratosthenes (tail eratosthenes), q==p+2]

```
### Non-list structures

Infinite data structures do not need to be lists.  Here is an example
using trees.  It builds possible paths of `Moves`: `LeftTurn`, `Ahead`
and `RightTurn`.
```haskell

data Move = LeftTurn | Ahead | RightTurn deriving (Eq)
newtype Paths = Paths [(Move, Paths)]
type Path = [Move]

```
Note that a value of type `Paths` is a finite list, of length in the
range 0 to 3, of potentially infinite objects, with at most one
occurence of each `Move`, although this restriction is not recorded in
the type.  [Exercise: give type declarations that _do_ enforce the
restriction, and redefine validpaths in terms of the new types.  One
way to do this is with 8 (=2^3) constructors for `Paths`.]

We can check a `Path` is compatible with a collection `Paths`:
```haskell

compatible :: Paths   -> Path  -> Bool
compatible    _          []     = True
compatible    (Paths vs) (m:ms) = not (null ds)
                                  && compatible (head ds) ms
  where
    ds = [f | (n,f)<-vs, n==m]

```
We can express the rule that

> an `Ahead` must follow any `LeftTurn` or `RightTurn`, but any move
> may follow an `Ahead`

as compatibility with an infinite value:
```haskell

validpaths :: Paths
validpaths  = Paths [ (LeftTurn, aheadfirst)
                    , (Ahead, validpaths)
                    , (RightTurn, aheadfirst) ]
  where
    aheadfirst  = Paths [(Ahead, validpaths)]
    
```
Then a function to test validity is easy to define:
```haskell

valid :: Path -> Bool
valid = compatible validpaths

```
[1]: <https://yorsearch.york.ac.uk/permalink/f/1kq3a7l/44YORK_ALMA_DS51343067510001381> Graham Hutton, "Programming in Haskell", 2nd edition.

[2]: <https://hoogle.haskell.org/> "Hoogle".

[3]: <https://doi-org.libproxy.york.ac.uk/10.1017/S0956796808007004> Melissa E. O'Neill, "The Genuine Sieve of Eratosthenes", J. Functional Programming, Volume 19, Issue 1, January 2009, pp. 95-106.
```haskell

