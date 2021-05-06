```haskell
module TypeClass where
import Data.Foldable

```
# Type classes
# Jeremy Jacob 22 Jan 2020

## Why look at type classes?

So far we have restricted ourselves to the **pure** part of Haskell,
but effects (sometimes called _side effects_) are essential.  As we
have said before, _ad_ _hoc_ effects are not compatible with lazy
evaluation.  However, **monadic** effects are compatible with lazy
evaluation.  In Haskell monadic effects are provided by a type class
called `Monad`, and so we need to understand more about type classes
to understand how to safely use effects.  We will return to `Monad`,
and its relatives in a different lecture, in Block 4.

Type classes provide a mechanism something like that of _interfaces_
in object-oriented languages, with similar advantages.  If a type or
type-constructor is an instance of a type class it guarantees to the
type system that various values have been defined relative to the type
or type constructor.  In addition type classes also promise various
properties.

## The example of `Eq`

An example we have met many times already is the `Eq` type class.  Its
description is given on [the Haskell.Org web pages](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Eq).  Things to note:
1. The type class takes a _type_ parameter, `a`.  In the jargon, the
   type class `Eq` has a parameter of **kind** `*`.
2. There are a number of methods (in this case, two: `(==)` and
   `(/=)`) associated with the class.
3. There are a number of minimal complete definitions (in this case,
   either method is enough on its own to define the other).
4. There are a number of laws _expected_ to hold of the methods;
   however these cannot be enforced by the system, instead it is up to
   the programmer to ensure that they hold.  (The programmer could
   demonstrate the laws by **proof**: see the lecture on _Reasoning_.
   Alternatively, confidence in the laws could be increased by
   property-based testing.)
5. There is a list of types defined in the Haskell base that are
   instances of the class (in this case, very many, but none being
   function types).

If we want _structural_ equality, then the Haskell system can
**derive** the appropriate definitions, if asked. An example:
```haskell
newtype TwoDVector = TwoDVector (Int, Int) deriving Eq
```
Values `TwoDVector (v, w)` and `TwoDVector (x, y)` are equal exactly
when `v==x && w==y`.

However, we sometimes want something else.  For example, if we
represent sets as lists we would like `[0,1]`, `[1, 0]`, `[0, 0, 1]`,
and `[1, 0, 0, 1]` to be considered equal, as they all represent the
set `{0, 1}`, but we do not get that from structural equality.
However, we can achieve this by declaring a new clone of the list
type, and defining equality for ourselves.
```haskell
newtype Set a = Set [a]
```
In order to define equality we first define the "Is a subset or equal"
predicate on two sets.
```haskell
subseteq :: Eq a => Set a -> Set a -> Bool
(Set xs) `subseteq` (Set ys) = all (`elem` ys) xs -- subseteq.0
```
Now we can instantiate the type class `Eq` with `Set a`, as long as
`Eq a` holds.
```haskell
instance Eq a => Eq (Set a) where
  ss == ts = ss `subseteq` ts && ts `subseteq` ss -- Set.(==).0
```
We ought to prove the laws, but do not do so here.

> Unordered, repetitive lists are **not** a good way to implement
> sets.  A better way is to use [the base library module `Data.Set`](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Set.html).

## The examples of `Foldable` and `Monoid`

Now we look at two type classes that can play together.  We have seen
that `foldr` and `foldl` capture a very common pattern of evaluation
over lists, but they are more general than that.  They are methods of
[the type class `Foldable`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Foldable).

In this case there are ten methods, and it is sufficient to define
either of two of them (`foldMap`, which combines folding a monoid
instance with mapping over the structure, or `foldr`) to instantiate
the type class.  The actual type of `foldr` is:
```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```
Here `t` is of **kind** `* -> *`, and called a _type constructor_, rather
than of kind `*`, a _type_, as we have for `Eq`.  There is a long list
of instances provided in the Haskell base, although not quite as many
as for `Eq`.  We are familiar with case of `t` being the list
constructor, `[...]`.  Again, as for `Eq`, there is a list of
properties that the methods must provide.  You should study the
example definition of `foldr` for trees given in the commentary in the
documentation.

The type of `foldMap` is more interesting, as it involves [a second
type class, `Monoid`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Monoid).
Rather than look at this we will look at a simpler function, `fold`,
available by importing the base library module `Data.Foldable`.
`fold` works like `foldr` and `foldl` but does not have parameters for
the operator or constant! Consider the types of these functions:
```haskell
fold  :: (Foldable t,    Monoid m          ) => t m -> m
foldr ::  Foldable t  => (a -> b -> b) -> b  -> t a -> b
foldl ::  Foldable t  => (b -> a -> b) -> b  -> t a -> b
```
The functions `foldr` and `foldl` have two parameters to represent the
operation and constant, as well as a parameter to represent the
structure over which the fold is occuring; function `fold` only has the
structure parameter, but instead its contents must satisfy `Monoid m`.

A _monoid_ is an abstract algebraic structure that is a specialisation
of a more general algebraic structure, _semigroup_.  In Haskell, this
is represented by making [type class `Monoid`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Monoid) depend on [type class
`Semigroup`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Semigroup). Semigroups have a binary operator, always called `(<>)` in
Haskell, and this operator must be associative.  In addition, `Monoid`
introduces a value, `mempty` (for "Monoid empty", or neutral, element)
which must be the identity of `(<>)` and a function
`mconcat :: Monoid m => [m] -> m`.  (It also introduces an alias,
`mappend`, for `(<>)`, but we will not use this alias.)

Many structures are instances of monoids, for example:
* Addition `(+)` of numbers, with identity `0`.
* Multiplication `(*)` of numbers, with identity `1`
* Conjunction `(&&)` of Booleans, with identity `True`.
* Disjunction `(||)` of Booleans, with identity `False`.
* Concatenation `(++)` of lists, with identity `[]`.
* Composition `(.)` of functions, with identity `id`.

There are many, many other examples, including addition and
multiplication of matrices.

We can define values _once_, for any monoid, using the names `(<>)`
and `mempty`, and then they will work for any monoid instance.  For
example,
```haskell
mconcat :: Monoid m => [m] -> m
mconcat = foldr (<>) mempty
```
Now `mconcat ["hello", " ", "world", "!"] ~~> "hello world!"` and
`mconcat [(*2), (*3)] ~~> (*6)` without us having to specify that we
mean `(++)` and `[]` or `(.)` and `id`: the appropriate values are
extracted from the instance declaration for the type.

Several useful functions come along with `Foldable`, such as
```haskell
all, any :: Foldable t => (a -> Bool) -> t a -> Bool
```

Both of these are defined in the module `Data.Foldable`, but available
through `Prelude` without the need to explicitly import `Data.Foldable`.

> `Prelude` imports selected functions from `Data.Foldable`, and
> re-exports them.  It does this for several other modules, too.

The question now arises: what is the value of `mconcat [True, False]`?
Is it `False`, as the conjunction monoid dictates, or `True`, as the
disjunction monoid dictates?  We can ask a similar question about
`mconcat [2, 3, 4]`.  This conundrum is solved by saying: _neither_.
Instead, two clones of `Bool` are introduced, called `All` and `Any`,
for conjunction and disjunction respectively.  The definition of `All`
as a monoid is:
```haskell
newtype All = All {getAll :: Bool}
instance Semigroup All where
  (All b) <> (All c) = All (b && c)
instance Monoid All where
  mempty = All True
```
(Typically the definition of `All` would also derive some useful type
classes, such as `Eq` and `Ord`.)

Now we can define values such as:
```haskell
all' :: [Bool] -> Bool
all' = getAll . mconcat . map All
```
(The prime in `all'` is to distinguish this version from the one in
the `Prelude`; which is defined by the more terse and more general
`all = getAll . foldMap All`.  The `Prelude` version of `all` works
for any instance of `Foldable`, not just lists.)

The function `all'` embeds the list of Booleans into the world of
`All` (`map All`), folds using the appropriate monoid values
(`mconcat`), and then extracts the Boolean value from the result
(`getAll`).  Recall that because `All` is defined using `newtype` the
type conversions are optimised away in the runtime code, but the link
to the correct monoid remains bound to `mconcat`.

Now we can evaluate `all' [True, False]` to get `False`.

> It would be better to import [the base library `Data.Monoid`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html)
> to define `All` and its dual, `Any`, together with many useful functions.

## More type classes

There are many type classes defined in the Haskell base libraries.
Often the problem at hand is an instance of least one of them, and by
loading the appropriate library you can write much shorter, more
robust and efficient code.

In the next part, in Block 4, we will consider three related type classes:
* [`Functor`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor), that generalises `map`,
* [`Applicative`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Applicative), that allows us to deal with applying functions
  inside containers to values inside the same type of container, and,
  most importantly,
* [`Monad`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Monad), that gives us effects.
```haskell
