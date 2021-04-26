```haskell
module WhyFP where -- good practice to have a module header, but only
                   -- necessary if a library module

```
# [Jeremy Jacob][1] 4 Sep 2020
# Introductory material for UoY CS Level I module Software 3 (SOF3) on functional programming

Note: I have used [Markdown][2] syntax inside comments.  A Literate
Haskell markdownstyle is in preparation [3].  Until it is standard,
non-literate Haskell code that obeys some simple conventions can be
processed using `Markdownify.hs`

## Introduction: what is functional programming?

First we need to know what functional programming is!

1. A program is a **single expression** to be evaluated.
2. an expression is either:
    1. a value denotation [examples: `42`, `'Z'`, `"Hello world!"`,
       `True`, `(\x->(\y->x))`],
    2. a name of a value [`pi`, `(+)`, `map`], or
    3. an application of an expression denoting a function to a second
       expression [`succ 5`, `(+) 1`, `((+) 1) 5`].
3. expressions are evaluated in a context of definitions: associations
   of names to values

Examples of definitions in Haskell syntax:
```haskell

lengthOfWeek = 7 -- number of days in a week
daysInNormalYear = 4*30 + 7*31 + 28 -- 30 days hath September, April,
                                    -- June, and November, All the
                                    -- rest have 31, But February's
                                    -- 28,...
daysInLeapYear = daysInNormalYear + 1 -- ...The LEAP YEAR, which comes
                                      -- once in four, Gives February
                                      -- one day more.
hw = "Hello World!" -- a name for the string "Hello World!"
circumference = \r -> 2 * pi * r -- a name for the function that
                                 -- computes the circumference of a
                                 -- circle with radius r
(>|) = \x->(\y->x) -- a name for a function that returns a function
                   -- that returns the value first thought of.
                   -- Examples: ((>|) 5) 'Z' ~~> (\y->5) 'Z' ~~> 5 ;
                   -- ((>|) 5) False ~~> (\y->5) False ~~> 5

```
### Syntactic aside:

In Haskell, names must either:

1. start with a lower-case letter, and may be followed any combination of
   letters, digits, primes (`'`) and underscores (`_`), or
2. be a sequence of symbols, drawn from a restricted list, surrounded
   by brackets (`(`, `)`).

Names that start with an upper-case letter are treated as values of
various kinds: for example they type `Bool` with values `True` and
`False`.

Haskell function application is *left-associative*, so, for example:
`(>|) 5 'Z'` means `((>|) 5) 'Z'`

Special rule for names of the second kind: they may be used *infix*
with the brackets removed.  Examples:

* `3 + 5`    means `(+) 3 5`, which means `((+) 3) 5`;
* `5 >| 'Z'` means `(>|) 5 'Z'`, which means `((>|) 5) 'Z'`.

We will now adopt these simplifications.  There are many more
syntactic simplifications in Haskell that we will meet later.

## REASON 1: Value-orientation

Look again at the second clause describing expressions (2.2): "a name
of a _value_".  A _value_, **not** a location that contains a value
nor an object.

This class of languages was named **value-oriented** by Martin
Odersky.  Languages in this class have the property of being
**referentially transparent**: all occurences of a name refer to the
same value.

Referential transparency gives simple underpinnings to the language.
This has a number of consequences.

1. It much easier to argue about the results of programs. Typically we
   use equational reasoning (as you will have learnt in your school
   algebra lessons) and induction (to cope with recursion).
2. The simple mathematical structure means that it is easy to encode
   many useful algebraic structures in the libraries.  (We will use
   semigroups, monoids, monads and others.)

Sometimes these languages are called **pure**, because they have no
_ad hoc_ side effects.

## REASON 2: All values are first class

Another important feature of functional programming languages is that:
**all values are first-class**: in particular functions can have
function arguments and results.

Example:
```haskell

twoop = \f->(\g->(\a->(\b->(\c->g (f a b) c))))

```
`twoop` has a function argument, `f`, and returns a function that has a
function argument `g`.  Here are example expressions using `twoop`:
`twoop` is sometimes called a **higher-order function**.
```haskell

exp1 = twoop (+) (*) 1 2 3 -- (1+2) * 3 ~~> 9
exp2 = twoop (*) (+) 1 2 3 -- 1*2 + 3   ~~> 5

```
Syntactic aside: Haskell programmers hate writing brackets, so Haskell
allows pattern matching syntax for function definitions:
```haskell

twoop' f g a b c = g (f a b) c -- the values denoted by twoop and
                               -- twoop' are identical.

```
First class functions allow us to easily abstract away computations
and computation structures, and instantiate them as necessary.  This
typically leads to much shorter, more regularly structured code than
in languages without first class functions.  Shorter,
better-structured code is easier to maintain.

This contribution to software engineering practices, among others, is
why several companies (eg Facebook, several banks) have adopted
Haskell for mission critical tasks.

## REASON 3: Laziness for better modularity

Some functional programming languages are **lazy**, all function calls
are "Call-by-need" (other languages are *eager*; functions calls may
be Call-by-name or Call-by-value).  Haskell is a lazy language.

Laziness is an evaluation strategy that only evaluates an expression
if it is needed; and only the part of an expression that is
needed. This allows us to separate control and data in new ways.

Only pure languages may be lazy.

Here is an example of a recursive function that generates a list of
the first n even numbers.  (Can you see why the last even number to be
produced needs to be `2*(n-1)`?)
```haskell

first_n_evens'' n = if n==0
                    then []
                    else first_n_evens'' (n-1) ++ [2*(n-1)]

```
Syntactic aside: In Haskell we can replace the if-then-else
structure by **patterns** that test the structure of
the arguments.
```haskell

first_n_evens' 0 = []
first_n_evens' n = first_n_evens' (n-1) ++ [2*(n-1)] -- recursive case

```
Unfortunately, control and data-generation are mixed up in
`first_n_evens'`.

Laziness lets us separate control and data, getting better modularity.
```haskell

evens = [2*n | n<-[0..]] -- Data: the infinite list of elements of the
                         -- form 2*n, where n is drawn from the
                         -- infinite list of natural numbers.
first_n_evens n = take n evens -- Control is through the function
                               -- `take', found in the Prelude, a
                               -- library loaded by default.

```
Having data as an infinite list does not matter, as laziness means
that the control function, `take`, does not attempt to look at all of
it.

Laziness is related to **memoisation**, which in turn is related to
**dynamic programming**.

## REASON 4: powerful type systems

An optional feature of any programming language is to have a
statically decidable type system.  Type systems can protect the
programmer from a large number of errors, at the cost of some freedom
to the programmer.

A very powerful, but flexible, family of type systems is available for
functional languages, decended from the original _Hindley-Milner type
system_.

Haskell uses a statically decidable extension to the standard
Hindley-Milner system, that gives a lot of protection with relatively
little loss of freedom.  It also has a number of optional extensions
which increase either protection or flexibility or both, including
some that are no longer decidable, and need heuristic, and even
human-assisted, theorem proving.

Here are the types of some of the values we have defined earlier:
```haskell

lengthOfWeek :: Integer
hw :: [Char] -- String is a synonym for [Char]
(>|) :: a -> b -> a
twoop :: (a -> b -> c) -> (c -> d -> e) -> a -> b -> d -> e
evens :: (Enum c, Num c) => [c]
first_n_evens :: (Enum a, Num a) => Int -> [a]

```
Note: The function-type constructor, `(->)`, is right-associative.

These types tell us that
1. `lengthOfWeek` is an Integer.
2. `hw` is a list of characters (that is, a string)
3. `twoop` takes two functions, both having two arguments; the types
   of the result of the first and first argument of the second must be
   identical (`c`).  twoop takes a further three parameters, the types of
   the first two of these must be identical to the types of the arguments
   to the first function (`a` & `b`), and the third must be identical to
   the that of the second argument to the second function (`d`).  The
   type of the result is the same as the result type of the second
   function (`e`).
4. `evens` is a list of any type that has both the attributes:
    + it is an enumerable type
    + it is a numeric type
    The actual type is fixed by the context.
5. `first_n_evens` is a function that takes an Integer, and returns a
   list of any type that is both enumerable and numeric.

## REASON 5: Difference

The features of strongly-but-flexibly-typed, lazy, higher-order,
value-oriented languages make them very powerful, but also very
different to the languages you already know.

Learning a language that is radically different to those you already
know will give you a deeper understanding of computation, of
programming languages, and change how you program in any language.  I
have come across people who think that this is the **best reason** for
learning such a language.

However, to appreciate this, you will need to change structures in
your brain.  This is one of the one of the most energy-intensive
things you can do.  So be sure to study **fun**ctional programming
with a cup of tea in one hand and a slice of cake in the other.

Enjoy the journey!

-----------------------------------------------------------------------

[1]: https://www.cs.york.ac.uk/people/?group=Academic%20and%20Teaching%20Staff&username=jeremy

[2]: https://www.markdownguide.org/

[3]: https://gitlab.haskell.org/ghc/ghc/-/wikis/literate-markdown
```haskell
