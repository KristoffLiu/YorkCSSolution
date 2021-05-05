```haskell
module Reasoning where

```
# Reasoning about programs
# Jeremy Jacob, 15 Jan 2021

## Reading
[Hutton, Chapter 16][1] covers this material.  [Hutton][1] uses a
slightly different notation to that used here.

## Introduction

A major advantage of pure functional languages over imperative
languages is that it is much easier to reason about what value an
expression has.  The main method of reasoning is _equational_, with
generalised _induction_ to deal with recursive definitions.

**Equational reasoning** is exactly what you learned at school to deal
with simple algebraic simplifications of numerical expressions.  Now
we are dealing with expressions of more complex types, such as
`[a] -> [a]` and `Maybe Int`.

Similarly, **structural induction** extends the idea of induction
 (studied in THE1) from natural numbers to other, recursive types,
 such as `[a]`.

## A trivial example

Here is a simple theorem about reversing the order of a pair.
Consider the function:
```haskell
rotate :: (a, b) -> (b, a)
rotate (x, y) = (y, x) -- rotate.0
```
where we have labelled the lines (all one of them!) of the definition
for later reference.

We can prove that
```haskell
rotate . rotate == (id :: (a, b) -> (a, b))
```
That is, the function `rotate . rotate`, which has type `(a, b) -> (a,
b)`
is equal to the identity function restricted to pairs.

Note that the theorem statement, while true, generates a type error
because functions are not in the type class `Eq` (see THE3).  We will
partly address this problem later.

We can rewrite the statement of this theorem using the
_function-extensionality_ version of equality.  This principle is
closely related to η-reduction (see the Lambda Calculus):
```haskell
∀ f, g :: a -> b {(f == g) == (∀ x::a {f x == g x})}
```
It says that two functions are equal exactly when the result of
applying them to equal arguments is equal. Using this principal our
theorem becomes:
```haskell
∀ p :: (a, b) {(rotate . rotate) p == id p}
```
Note that we don't need to explicitly specialise `id`, as the type
comes from its argument.

There are two cases of `(a,b)` that we need to consider:
1. elements that compute to an actual pair, and
2. elements that represent failed computations.

Case 1 is the easiest.  We will need to refer to the definitions of
`id` and function composition, `(.)`, repeated here, with labelled
lines:
```haskell
id :: a -> a
id x = x -- id.0

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)  -- (.).0
```
Now we can present an annotated proof for Case 1:
```haskell
(rotate . rotate) (x, y)
== -- (.).0
rotate (rotate (x, y))
== -- rotate.0
rotate (y, x)
== -- rotate.0
(x, y)
== -- id.0
id (x, y)
```
The annotations, or **hints**, on the equalities tell us which
definitions were used to justify the step.  Note that the last
justification, to introduce `id`, is used _right-to-left_, rather than
_left-to-right_.  Another thing that we have omitted from the
annotations is where in the first expression we are applying the rule;
the only interesting example in this proof is the first annotation
`rotate.0`, which could apply to either occurence of `rotate`.  If we
wanted a machine to check this proof we would need to add the missing
information, but people can usually work it out for themselves.

## Dealing with failed computations

There are two ways that computations can fail:
1. Infinite loops
2. Missing definitions.  These can be divided into implicit and explicit.

Below, `bottom` is the prototypical infinite loop, while `caput` and
`caput'` illustrate an implicit undefined computation and an explicit
undefined computation, both in the case of the empty list.
```haskell
bottom :: a
bottom = bottom

caput, caput' :: [a] -> a
caput (x:_) = x
caput' []    = undefined
caput' (x:_) = x
```
In the theory we do not distinguish why a computation has failed.  We
will write all of these as `⊥` (`_|_` in plain text; pronounced
“bottom”, and hence the name of the definition above).  A function is
called **strict** if applying it to `⊥` results in `⊥`.  Both `rotate`
and `id` are strict.  An example of a non-strict function is `const
0`.

Functions are strict if they **must** examine their arguments to
decide what to do next and non-strict otherwise.  The function `const
0`
just needs to know that it has been applied to return `0`, it does
not need to examine its argument at all.  The function `rotate` needs
to extract the two components of the pair in order to swap them
around, and so needs to examine its argument (but does not need to
examine the components themselves).  While `id` does not examine its
argument, it is strict as it passes on ⊥ unchanged.

So to finish our proof of
```haskell
(rotate . rotate) == (id :: (a, b) -> (a,b)
```
we need to show `(rotate . rotate) ⊥ == id ⊥`.
```haskell
(rotate . rotate) ⊥
== -- (.).0
rotate (rotate ⊥)
== -- rotate strict
rotate ⊥
== -- rotate strict
⊥
== -- id.0 or id strict
id ⊥
```
hence we can conclude: `rotate . rotate == id`

So we now know that `rotate . rotate` and `id` always give the same
result for the same correctly-typed input; but that leaves us with the
problem of which of the two to use in practice.  A different type of
reasoning tells us that both are `O(1)` for time, but
`rotate . rotate` has a larger leading constant than `id` does, so that we
should prefer `id`.  Something similar holds for space usage.
Reasoning about resource usage in lazy languages is more complex than
in eager languages, and we do not discuss it formally in this module.
For a fuller discussion see [Bird & Gibbons][2].

## Limited checking of proofs

If we write proofs by hand there is no guarantee that we will not make
errors.  Writing a full-blown proof checker, let alone an automated
proof assistant is well beyond the scope of this module.

> There is a Stage 3/4 module in preparation, "Assurance and Proof"
>  (PROF), that looks at reasoning using the theorem proving assistant
>  [Isabelle](https://isabelle.in.tum.de/), which itself is structured
>  as a typed functional language, and written in a typed functional
>  language.  There is a book that describes how to use Isabelle, and
>  Part 1 is about the verification of functional programs [Nipkow &
>  Klein][3].

However, we can easily embed support for syntax and type-checking, and
a _little_ bit more, by introducing a suitable data type and
functions.  We will write equality at the outermost level as `(:=:)`,
which we pronounce "equivalence" (we can still use `(==)` within the
expressions about which we are reasoning).
```haskell
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
```
We can make this a `Foldable`, and take advantage of `Prelude`
functions defined for `Foldable` types to define a **test** when the
underlying type is in the `Eq` class.
```haskell
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL QED        = True
testPL (p :=: pl) = all (==p) pl
```
We can now use this to layout the proof in the section above:
```haskell
rotateSelfInverse :: (a, b) -> ProofLayout (a, b)
rotateSelfInverse (x, y) =
  (rotate . rotate) (x, y)
  :=: -- (.).0
  rotate (rotate (x, y))
  :=: -- rotate.0
  rotate (y, x)
  :=: -- rotate.0
  (x, y)
  :=: -- id.0
  id (x, y)
  :=: QED
```
The universal quantification, `∀ p :: (a,b) {...}` is encoded as the
parameters of `rotateSelfInverse`. `rotateSelfInverse` is a convenient
name of the theorem when used as a hint in later proofs.

The Haskell tools (for example, GHCi), can tell us that the sytntax
and types are right, but not that the values are all equal.  That fact
we can **test**, by calling `testPL . rotateSelfInverse` on suitable
values of `p`; a suitable value is any concrete example whose elements
have a type in class `Eq`.  Here is **ONE** example; a proper test
regime has many.
```haskell
testRSI :: Bool
testRSI= testPL (rotateSelfInverse (0, 'a'))
```
Here is a **BAD** proof, that passes the syntax and type checks:
```haskell
badProof :: a -> a -> [a] -> ProofLayout a
badProof y z ys =
  head (y:z:ys)
  :=: -- Magic!
  z
  :=: QED

testBadProof, testBadProof' :: Bool
testBadProof  = testPL (badProof 2 3 [4..9])
testBadProof' = testPL (badProof 2 2 [4..9])
```
the first of these, `testBadProof` detects that the proof is bad, but
`testbadProof'` fails to find the error.  It is necessary to test
widely!

> This is taking us towards the notion of **property based testing**,
> which we discuss elsewhere.

Note that there is no way, without a **LOT** more work to build a
meta-level tool, of checking that the hints are right.

## Reasoning with induction
Consider the property _HomAppendAdd_:
```haskell
∀ xs, ys :: [a] {length (xs ++ ys) = length xs + length ys}
```
This is an important property: it tells us that the algebra of `(++)`
over `[a]`, for any type `a`, is similar to the algebra of `(+)` over
the Natural numbers: both are monoids.  The technical term for this
sort of relationship is _homomorphism_.

To prove it, we will need to use **structural induction**.  The idea
is the same as that of **mathematical induction** you studied in THE1.
Mathematical induction is just a special case of structural induction.

The key principle of induction is that we may assume the theorem is
true when considering _subcases_ of the case we are currently
considering.  This is similar to considering that a function works on
_subcases_ when designing a recursive function.  In both induction and
recursion we need at least one case that has no subcases: these are
called _base cases_.

Consider any recursive data type, such as `Nat`, or `RGBS`:
```haskell
data Nat = Zero | Succ Nat
data RGBS a b c d = RedLeaf a | GreenLeaf b
                  | Branch (RGBS a b c d) c (RGBS a b c d)
                  | Stem d (RGBS a b c d)
```
We can identify _base cases_: `Zero` for `Nat`, and `RedLeaf x` and
`GreenLeaf y` for `RGBS`.  We can also see recursive cases, that
require inductive steps: `Succ n` for `Nat`, and `Branch left z right`
and `Stem w trunk` for `RGBS`.  The type `Nat` generates exactly the
same induction scheme as that for natural numbers.  The scheme for the
type `RGBS` is more complex, with two base cases, an induction case
with one sub-case and an induction case with two sub-cases.

In the case of lists, which we will need for _HomAppendAdd_, we have:
* Prove the theorem for the list `[]` (a base case0).
* Prove the theorem for a non-empty list, `(x:xs)`, assuming the
  theorem holds for `xs` (an inductive case).
* Prove the theorem for `⊥` (a type of base case).

We can now prove the theorem _HomAppendAdd_.
For convenience we repeat the definitions:
```haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys             -- (++).0
(x:xs) ++ ys = x : (xs ++ ys) -- (++).1

length :: [a] -> Int
length []     = 0             -- length.0
length (_:xs) = 1 + length xs -- length.1
```
```haskell
homAppendAdd :: [a] -> [a] -> ProofLayout Int
homAppendAdd [] ys =
  length ([] ++ ys)
  :=: -- (++).0
  length ys
  :=: -- identity of addition
  0 + length ys
  :=: -- length.0
  length [] + length ys
  :=: QED
homAppendAdd (x:xs) ys =
  length ((x:xs) ++ ys)
  :=: -- (++).1
  length (x : (xs++ys))
  :=: -- length.1
  1 + length (xs ++ ys)
  :=: -- induction step
  1 + (length xs + length ys)
  :=: -- associativity of addition
  (1 + length xs) + length ys
  :=:
  length (x:xs) + length ys
  :=: QED
```
Note how the proof uses the two key properties of `(+)` as a monoid:
it has an identity and is associative.

For the case where `xs==⊥`, we have
```haskell
length (⊥ ++ ys)
== -- (++) is strict in its first argument
length ⊥
== -- length is strict
⊥
== -- (+) is strict in both arguments
⊥ + length ys
== -- length is strict
length ⊥ + length ys
```

## Using an inefficient, but clear, definition to justify a less clear, but more efficient definition
Here is an another important use case for proof.  Consider the two
definitions:
```haskell
rv0, rv1 :: [a] -> [a] 
rv0 []     = []           -- rv0.0
rv0 (x:xs) = rv0 xs ++ [x] -- rv1.1

rv1 = rv1' []
  where
    rv1' ys []     = ys             -- rv1'.0
    rv1' ys (x:xs) = rv1' (x:ys) xs -- rv1'.1
```
It should be fairly easy to see that `rv0` reverses its input list.
Less easy is to see that it takes quadratic time: concatenation `(++)`
is linear in its first argument, and concatenation is called a linear
number of times.  The function `rv1` also reverses its input, but it
does so in linear time; hence it is the better of the two algorithms.
However it is not so clear that `rv1` actually reverses its input.  We
can prove it by proving that `rv1 == rv0`.

The proof that `rv0 == rv1` is too involved for here; see [Hutton,
Section 16.6][1].

Similar proofs are used to justify optimisations inside a compiler,
which is another important use case.

> Note that `rv0` is a right-fold, and `rv1` is left-fold, both
> written out in full.  However, to deal with the definitions in the
> form of folds we need first to have at hand many theorems about
> folds.  This is beyond the scope of introductory material.
>
> For that matter, both `(++)` and `length` can be expressed as folds,
> and we could prove a very general theorem about folds over monoids,
> of which `homAppendAdd` is special case.

## References

[1]: <https://yorsearch.york.ac.uk/permalink/f/1kq3a7l/44YORK_ALMA_DS51343067510001381> Graham Hutton,
"Programming in Haskell", 2nd edition, Cambridge University Press, 2016.

[2]: Richard Bird and Jeremy Gibbons,
"Algorithm Design with Haskell", Cambridge University Press, 2020 [avaliable as an e-book from the JBML]

[3]: <http://www.concrete-semantics.org/> Tobias Nipkow and Gerwin Klein,
"Concrete Semantics", Springer, 2014.

## Appendix: reasoning about imperative programs

Several methods have been discovered for reasoning about imperative
programs.  These include:

* **Operational semantics** where high-level commands are described by
    showing how to execute them on a `machine` built out of
    mathematical structures that abstract from real machines.  Many of
    these structures are functions.
* **Denotational semantics** which essentially asks us to first
    compile our code into a functional program.
* **Hoare logic** which treats programs as statements in a _formal
    logic_.  The logic describes how a program relates the predicate
    describing the desired final states, the _postcondition_, to a
    predicate describing the initial states, the _precondition_, from
    which the execution of the program is guaranteed to reach a state
    described by postcondition.  Dijkstra showed how to cast Hoare's
    methods as a kind of denotational semantics, using predicate
    transformers, which are like functions over predicates.

Note that:
1. In all of these, loops give rise to the most complex part of the
   theory (why should that be the case?).  With equational reasoning
   in functional programs we can reach for the relatively simple
   mechanism of structural induction.
2. To deal with changes to stores (or, "memory") resulting from an
   assignment, these methods use _substitution_.  This technique
   underpins the way that functional programs are executed, and terms
   in our proof are instantiated.
```haskell
